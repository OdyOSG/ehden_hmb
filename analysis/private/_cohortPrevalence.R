# Cohort Covariates -----------

verboseSave <- function(object, saveName, saveLocation) {

  savePath <- fs::path(saveLocation, saveName, ext = "csv")
  readr::write_csv(object, file = savePath)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(savePath)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(saveLocation), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()
  invisible(savePath)
}

cohortCovariates <- function(con,
                             cohortDatabaseSchema,
                             cohortTable,
                             cohortKey,
                             covariateKey,
                             timeA,
                             timeB,
                             outputFolder) {

  cli::cat_rule("Build Cohort Covariates")

  targetId <- cohortKey$id
  eventId <- covariateKey$id
  #sql to get cohort covariates - period prevalence change
  sql <- "
    SELECT
      t.cohort_definition_id AS target_cohort_id,
      e.cohort_definition_id AS covariate_cohort_id,
      count(e.subject_id) as n
    FROM (
      SELECT *
      FROM @cohortDatabaseSchema.@cohortTable
      WHERE cohort_definition_id IN (@targetId)
    ) t
    JOIN (
      SELECT *
      FROM @cohortDatabaseSchema.@cohortTable
      WHERE cohort_definition_id IN (@eventId)
    ) e
    ON t.subject_id = e.subject_id
    AND (
      e.cohort_start_date BETWEEN
          DATEADD(day, @timeA, t.cohort_start_date) AND
          DATEADD(day, @timeB, t.cohort_start_date)
      OR e.cohort_end_date BETWEEN
          DATEADD(day, @timeA, t.cohort_start_date) AND
          DATEADD(day, @timeB, t.cohort_start_date))
    GROUP BY t.cohort_definition_id, e.cohort_definition_id
"
  # Render and translate sql
  cohortCovariateSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    targetId = targetId,
    eventId = eventId,
    timeA = timeA,
    timeB = timeB
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  #run query on connection
  tb <- dplyr::tbl(con, dbplyr::in_schema(cohortDatabaseSchema, cohortTable)) %>%
    dplyr::filter(cohort_definition_id %in% targetId) %>%
    dplyr::count(cohort_definition_id) %>%
    dplyr::collect() %>%
    dplyr::right_join(cohortKey, by = c("cohort_definition_id" = "id")) %>%
    dplyr::rename(id = cohort_definition_id) %>%
    dplyr::select(id, name, n) %>%
    dplyr::arrange(id)


  #run query on connection
  cohortCovTbl <- DatabaseConnector::querySql(con, sql = cohortCovariateSql) %>%
    rename(
      cohortId = TARGET_COHORT_ID,
      covariateId = COVARIATE_COHORT_ID,
      count = N
    ) %>%
    dplyr::left_join(
      tb, by = c("cohortId" = "id")
    ) %>%
    dplyr::rename(cohortName = name) %>%
    dplyr::left_join(
      covariateKey, by = c("covariateId" = "id")
    ) %>%
    dplyr::rename(covariateName = name) %>%
    dplyr::mutate(
      pct = count / n
    ) %>%
    dplyr::select(
      cohortId, cohortName, covariateId, covariateName, count, pct
    )

  verboseSave(
    object = cohortCovTbl,
    saveName = paste("cohort_covariates", abs(timeA), abs(timeB), sep = "_"),
    saveLocation = outputFolder
  )


  invisible(cohortCovTbl)


}


executeCohortPrevalence <- function(con,
                                    executionSettings,
                                    analysisSettings) {

  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()


  ## get cohort Ids
  cohortKey <- analysisSettings$postIndexPrevalence$cohorts$targetCohort
  covariateKey <- analysisSettings$postIndexPrevalence$cohorts$drugCohorts

  timeA <- analysisSettings$postIndexPrevalence$prevalenceTimeWindow$startDays
  timeB <- analysisSettings$postIndexPrevalence$prevalenceTimeWindow$endDays

  #set ids
  cohortId <- cohortKey$id
  covId <- covariateKey$id


  #Start execution talk
  cli::cat_boxx("Building Post-Index Covariates")
  cli::cat_line()

  tik <- Sys.time()
  #loop on time Windows
  for (i in seq_along(timeA)) {
    # Job Log
    cli::cat_rule(glue::glue("Post-Index Analysis Job ", i))
    cli::cat_bullet("Running Post-Index Analysis at window: [",
                    crayon::green(timeA[i]), " - ",
                    crayon::green(timeB[i]), "]",
                    bullet = "info", bullet_col = "blue")
    cat_cohortId <- paste(cohortId, collapse = ", ")
    cli::cat_bullet("Building prevalence for cohort ids:\n   ",crayon::green(cat_cohortId),
                    bullet = "info", bullet_col = "blue")
    cat_cohortId <- paste(covId, collapse = ", ")
    cli::cat_bullet("Using cohorts ids:\n   ", crayon::green(cat_cohortId),
                    bullet = "info", bullet_col = "blue")

    # Run post-index s
    cohortCovariates(con = con,
                     cohortDatabaseSchema = workDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortKey = cohortKey,
                     covariateKey = covariateKey,
                     timeA = timeA[i],
                     timeB = timeB[i],
                     outputFolder = outputFolder)


  }
  tok <- Sys.time()
  cli::cat_bullet("Execution Completed at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")
  invisible(tok)

}
