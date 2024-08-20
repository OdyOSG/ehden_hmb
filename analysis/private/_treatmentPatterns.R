# A. File Info -----------------------

# Task: Treatment Patterns
# Description: The purpose of the _treatmentPatterns.R script is to.....


# B. Functions ------------------------

## Post index Prevalence -------------------

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


# Covariate started and ended within time window
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
    GROUP BY t.cohort_definition_id, e.cohort_definition_id;
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
      pct = count / n,
      timeWindow = paste0(abs(timeA), "_" ,abs(timeB)),
      type = "within"
    ) %>%
    dplyr::select(
      cohortId, cohortName, covariateId, covariateName, count, pct, timeWindow, type
    )

  verboseSave(
    object = cohortCovTbl,
    saveName = paste("cohort_covariates", abs(timeA), abs(timeB), sep = "_"),
    saveLocation = outputFolder
  )

  invisible(cohortCovTbl)
}


# Covariate started at least on cohort start date + timeA and ended at least on cohort start date + timeB
cohortCovariates2 <- function(con,
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

  # SQL code
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
      e.cohort_start_date <= DATEADD(day, @timeA, t.cohort_start_date) AND
      e.cohort_end_date >= DATEADD(day, @timeB, t.cohort_start_date)
          )
    GROUP BY t.cohort_definition_id, e.cohort_definition_id;
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
      pct = count / n,
      timeWindow = paste0(abs(timeA), "_" ,abs(timeB)),
      type = "followUp"
    ) %>%
    dplyr::select(
      cohortId, cohortName, covariateId, covariateName, count, pct, timeWindow, type
    )

  verboseSave(
    object = cohortCovTbl,
    saveName = paste("cohort_covariates", abs(timeA), abs(timeB), sep = "_"),
    saveLocation = outputFolder
  )

  invisible(cohortCovTbl)
}


executePostIndexDrugUtilization <- function(con,
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

  piduFolder <- outputFolder[[1]]
  piduFolder2 <- outputFolder[[2]]

  ## get cohort Ids
  cohortKey <- analysisSettings$treatmentPatterns$cohorts$targetCohort
  covariateKey <- analysisSettings$treatmentPatterns$cohorts$drugCohorts

  timeA <- analysisSettings$treatmentPatterns$timeWindow$startDay
  timeB <- analysisSettings$treatmentPatterns$timeWindow$endDay

  #set ids
  cohortId <- cohortKey$id
  covId <- covariateKey$id

  #Start execution talk
  cli::cat_boxx("Building Post-Index Covariates")
  cli::cat_line()

  tik <- Sys.time()

  # Loop on time Windows
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


    cohortCovariates(con = con,
                     cohortDatabaseSchema = workDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortKey = cohortKey,
                     covariateKey = covariateKey,
                     timeA = timeA[i],
                     timeB = timeB[i],
                     outputFolder = piduFolder)

    cohortCovariates2(con = con,
                     cohortDatabaseSchema = workDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortKey = cohortKey,
                     covariateKey = covariateKey,
                     timeA = timeA[i],
                     timeB = timeB[i],
                     outputFolder = piduFolder2)

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


## Treatment Patterns -------------------------

prepSankey <- function(th, minNumPatterns, flag = c("6m","1y","2y","end")) {

#prepSankey <- function(th, minNumPatterns) {

  # treatment_pathways <- th %>%
  #   tidyr::pivot_wider(id_cols = person_id,
  #                      names_from = event_seq,
  #                      names_prefix = "event_cohort_name",
  #                      values_from = event_cohort_name) %>%
  #   dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
  #   dplyr::mutate(End = "end", .before = "n") %>%
  #   dplyr::filter(n >= minNumPatterns)

  treatment_pathways_in <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name,
                       unused_fn = min)

  treatment_pathways2 <- treatment_pathways_in[treatment_pathways_in$flag %in% flag, ]

  treatment_pathways <- treatment_pathways2 %>%
    #dplyr::filter(flag %in% flag) %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n") %>%
    dplyr::filter(n >= minNumPatterns)

  links <- treatment_pathways %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(treatment_pathways))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop') %>%
    dplyr::arrange(desc(value))

  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)
  data.table::setorder(links, cols = - "value")

  res <- list(
    'treatmentPatterns' = treatment_pathways,
    'links' = links,
    'nodes' = nodes
  )

  return(res)
}



executeTreatmentPatterns <- function(con,
                                     executionSettings,
                                     analysisSettings) {

  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  # make output folder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder)
  thHistoryFolder <- outputFolder[[1]]
  txPatFolder <- outputFolder[[2]]

  targetCohortId <- analysisSettings$treatmentPatterns$cohorts$targetCohort$id
  targetCohortName <- analysisSettings$treatmentPatterns$cohorts$targetCohort$name

  # list all treatment history files
  thFiles <- fs::dir_ls(thHistoryFolder, recurse = TRUE, type = "file")

  for (i in seq_along(thFiles)) {

    # read parquet file - get treatment history
    th <- arrow::read_parquet(file = thFiles[i])

    # # Extract common lead of directory
    # folder_label <- gsub(paste0(thHistoryFolder, "/"), "", thFiles[i]) %>%
    #   fs::path_dir()

    # Get the target cohort name
    file_label <- tools::file_path_sans_ext(basename(thFiles[i])) %>%
      gsub("th_", "", .)

    # Create output folder
    save_path <- fs::path(txPatFolder) %>%
      fs::dir_create()


    ## All time ----------------------------------------------

    # Create object to export

    patterns <- th %>%
      prepSankey(minNumPatterns = 30L)

    # Save file
    save_path_all <- fs::path(paste0(txPatFolder, "/all")) %>%
      fs::dir_create()
    file_name <- paste("sankey", targetCohortId[i], sep = "_")
    save_path2 <- fs::path(save_path_all, file_name, ext = "rds")
    readr::write_rds(patterns, file = save_path2)


    ## 6 months ----------------------------------------------

    # Create object to export
    patterns6m <- th %>%
      #dplyr::filter(flag %in% c("6m")) %>%
      #prepSankey(minNumPatterns = 30L)
      prepSankey(minNumPatterns = 30L, flag = c("6m"))

    # Save file
    save_path_6m <- fs::path(paste0(txPatFolder, "/6m")) %>%
      fs::dir_create()
    file_name <- paste("sankey", targetCohortId[i], sep = "_")
    save_path2 <- fs::path(save_path_6m, file_name, ext = "rds")
    readr::write_rds(patterns6m, file = save_path2)


    ## 1 year ----------------------------------------------

    # Create object to export
    patterns1y <- th %>%
      #dplyr::filter(flag %in% c("6m", "1y")) %>%
      #prepSankey(minNumPatterns = 30L)
      prepSankey(minNumPatterns = 30L, flag = c("6m", "1y"))

    # Save file
    save_path_1y <- fs::path(paste0(txPatFolder, "/1y")) %>%
      fs::dir_create()
    file_name <- paste("sankey", targetCohortId[i], sep = "_")
    save_path2 <- fs::path(save_path_1y, file_name, ext = "rds")
    readr::write_rds(patterns1y, file = save_path2)


    ## 2 years ----------------------------------------------

    # Create object to export
    patterns2y <- th %>%
      #dplyr::filter(flag %in% c("6m", "1y", "2y")) %>%
      #prepSankey(minNumPatterns = 30L)
      prepSankey(minNumPatterns = 30L, flag = c("6m", "1y", "2y"))

    # Save file
    save_path_2y <- fs::path(paste0(txPatFolder, "/2y")) %>%
      fs::dir_create()
    file_name <- paste("sankey", targetCohortId[i], sep = "_")
    save_path2 <- fs::path(save_path_2y, file_name, ext = "rds")
    readr::write_rds(patterns2y, file = save_path2)


    # Job log
    cli::cat_line()
    cli::cat_bullet("Saved file ", crayon::green(basename(save_path2)), " to:",
                    bullet = "info", bullet_col = "blue")
    cli::cat_bullet(crayon::cyan(save_path), bullet = "pointer", bullet_col = "yellow")
    cli::cat_line()

  }

  invisible(patterns)
}


## Time to Event -----------------------

prepTte <- function(con,
                    th,
                    workDatabaseSchema,
                    cohortTable,
                    targetCohorts) {

  cli::cat_line(crayon::blue("Extracting Survival Table..."))

  targetCohortIds <- targetCohorts$id
  targetCohortNames <- targetCohorts$name

  # get target cohort table
  sql <- "SELECT * FROM @write_schema.@cohort_table
          WHERE cohort_definition_id IN (@target_cohort_id);"  %>%
    SqlRender::render(
      write_schema = workDatabaseSchema,
      cohort_table = cohortTable,
      target_cohort_id = targetCohortIds
    ) %>%
    SqlRender::translate(con@dbms)

  targetTbl <- DatabaseConnector::querySql(connection = con, sql = sql)
  colnames(targetTbl) <- tolower(colnames(targetTbl))

  # Prep treatment history table to tibble
  dt <- th %>%
    tibble::as_tibble()

  # Create list to save results of survfit
  survFitList <- vector('list', 2)

  # Loop on targetCohortIds
  for (i in seq_along(targetCohortIds)) {

    tte <- targetTbl %>%
      # filter to cohort id
      dplyr::filter(cohort_definition_id == targetCohortIds[i]) %>%
      # join th and target table to determine censoring
      dplyr::inner_join(th, by = c("subject_id" = "person_id"), relationship = "many-to-many") %>%
      #identifying the event and convert time to years
      dplyr::mutate(
        event = dplyr::case_when(
          event_end_date < cohort_end_date ~ 1,
          TRUE ~ 0
        ),
        time_years = duration_era / 365.25
      ) %>%
      dplyr::filter(time_years <= 3) %>%
      dplyr::select(event_cohort_id, event_cohort_name, time_years, event)

    ## Get single lines
    singleLineStrata <- tte %>%
      dplyr::filter(
        !grepl("\\+", event_cohort_name)
      ) %>%
      dplyr::pull(event_cohort_name) %>%
      unique()

    ## Get top 4 multi-lines
    top4MultiLineStrata <-tte %>%
      dplyr::filter(
        grepl("\\+", event_cohort_name)
      ) %>%
      count(event_cohort_name) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:4) %>%
      dplyr::pull(event_cohort_name) %>%
      unique()

    ## Combine specified strata lines
    strataLines <- c(singleLineStrata, top4MultiLineStrata)

    ## All lines
    tteAll <- tte %>% dplyr::filter(event_cohort_name %in% strataLines)

    # Create the surv fit object
    survFitAll <- ggsurvfit::survfit2(
      survival::Surv(time_years, event) ~ event_cohort_name, data = tteAll
    )


    ## Single lines only
    tteSingle <- tte %>% dplyr::filter(event_cohort_name %in% singleLineStrata)

    ## Create the surv fit object
    survFitSingle <- ggsurvfit::survfit2(
      survival::Surv(time_years, event) ~ event_cohort_name, data = tteSingle
    )

    ## Add ggsurvfit objects to list
    survFitList <- list("All" = survFitAll,
                        "Single" = survFitSingle)

  }

  return(survFitList)
}


prepTteTables <- function(con,
                          th,
                          workDatabaseSchema,
                          cohortTable,
                          targetCohorts) {

  cli::cat_line(crayon::blue("Extracting Survival Table..."))

  targetCohortIds <- targetCohorts$id
  targetCohortNames <- targetCohorts$name

  # get target cohort table
  sql <- "SELECT * FROM @write_schema.@cohort_table
          WHERE cohort_definition_id IN (@target_cohort_id);"  %>%
    SqlRender::render(
      write_schema = workDatabaseSchema,
      cohort_table = cohortTable,
      target_cohort_id = targetCohortIds
    ) %>%
    SqlRender::translate(con@dbms)

  targetTbl <- DatabaseConnector::querySql(connection = con, sql = sql)
  colnames(targetTbl) <- tolower(colnames(targetTbl))

  #prep treatment history table to tibble
  dt <- th %>%
    tibble::as_tibble()

  # Loop on targetCohort Ids
  for (i in seq_along(targetCohortIds)) {

    tte <- targetTbl %>%
      # filter to cohort id
      dplyr::filter(cohort_definition_id == targetCohortIds[i]) %>%
      # join th and target table to determine censoring
      dplyr::inner_join(th, by = c("subject_id" = "person_id"), relationship = "many-to-many") %>%
      #identifying the event and convert time to years
      dplyr::mutate(
        event = dplyr::case_when(
          event_end_date < cohort_end_date ~ 1,
          TRUE ~ 0
        ),
        time_years = duration_era / 365.25
      ) %>%
      dplyr::filter(time_years <= 3) %>%
      dplyr::select(event_cohort_id, event_cohort_name, time_years, event)

    # Get single lines
    singleLineStrata <- tte %>%
      dplyr::filter(
        !grepl("\\+", event_cohort_name)
      ) %>%
      dplyr::pull(event_cohort_name) %>%
      unique()

    # Get top 4 multi-lines
    top4MultiLineStrata <-tte %>%
      dplyr::filter(
        grepl("\\+", event_cohort_name)
      ) %>%
      count(event_cohort_name) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1:4) %>%
      dplyr::pull(event_cohort_name) %>%
      unique()

    # Combine specified strata lines
    strataLines <- c(singleLineStrata, top4MultiLineStrata)


    tteAll <- tte %>% dplyr::filter(event_cohort_name %in% strataLines)
    tteSingle <- tte %>% dplyr::filter(event_cohort_name %in% singleLineStrata)


    # Create the surv fit object - All lines
    survFitAll <- ggsurvfit::survfit2(
      survival::Surv(time_years, event) ~ event_cohort_name, data = tteAll
    )

    # Create the surv fit object - Single lines only
    survFitSingle <- ggsurvfit::survfit2(
      survival::Surv(time_years, event) ~ event_cohort_name, data = tteSingle
    )

    # Place the surv fit table into the list - All lines
    survFitAllTbl <- ggsurvfit::tidy_survfit(survFitAll) %>%
      dplyr::select(time, `n.risk`, `n.event`, estimate:strata) %>%
      dplyr::mutate(
        targetCohortId = targetCohortIds[i],
        targetCohortName = targetCohortNames[i],
        line = "All"
      )

    # Place the surv fit table into the list - Single lines only
    survFitSingleTbl <- ggsurvfit::tidy_survfit(survFitSingle) %>%
      dplyr::select(time, `n.risk`, `n.event`, estimate:strata) %>%
      dplyr::mutate(
        targetCohortId = targetCohortIds[i],
        targetCohortName = targetCohortNames[i],
        line = "Single"
      )


    survFitTables <- dplyr::bind_rows(survFitAllTbl, survFitSingleTbl)

  }

  return(survFitTables)
}


executeTimeToEvent <- function(con,
                           executionSettings,
                           analysisSettings) {

  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  # prep output folder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder)
  thHistoryFolder <- outputFolder[1] # hard coded to get th folder
  tteFolder <- outputFolder[3] # hard coded to get tte folder

  targetCohorts <- analysisSettings$treatmentPatterns$cohorts$targetCohort
  targetCohortId <- targetCohorts$id
  targetCohortName <- targetCohorts$name


  # List all treatment history files
  thFiles <- fs::dir_ls(thHistoryFolder, recurse = TRUE, type = "file")

  # Loop through treatment history files
  for (i in seq_along(thFiles)) {

    # Read parquet file - Treatment history
    th <- arrow::read_parquet(file = thFiles[i])

    # Get target cohort name
    file_label <- tools::file_path_sans_ext(basename(thFiles[i])) %>%
      gsub("th_", "", .)


    # Job Log
    cli::cat_line()
    cli::cat_bullet(crayon::magenta("Execute time to discontinuation for Id: "),
                    targetCohortId[i], " (name: ",  targetCohortName[i], ")",
                    bullet = "pointer", bullet_col = "yellow")


    tmp <- targetCohorts %>%
      dplyr::filter(
        id == targetCohortId[i]
      )


    # Get TTE survfit objects (list)
    tteDat <- prepTte(con = con,
                      th = th,
                      workDatabaseSchema = workDatabaseSchema,
                      cohortTable = cohortTable,
                      targetCohorts = tmp)

    # Get TTE tables (data frame)
    tteDatTables <- prepTteTables(con = con,
                                  th = th,
                                  workDatabaseSchema = workDatabaseSchema,
                                  cohortTable = cohortTable,
                                  targetCohorts = tmp)


    # Create output folder
    save_path <- fs::path(tteFolder) %>%
      fs::dir_create()

    # Save TTE table
    file_name <- paste("tte", file_label, sep = "_")
    save_path2 <- fs::path(save_path, file_name, ext = "csv")
    readr::write_csv(tteDatTables, file = save_path2)

    # Save TTE survfit
    file_name <- paste("tte", databaseId, file_label, sep = "_")

    save_path <- paste0(save_path, "_rds") %>% fs::dir_create()

    save_path2 <- fs::path(save_path, file_name, ext = "rds")
    readr::write_rds(tteDat, file = save_path2)

    # Job Log
    cli::cat_line()
    cli::cat_bullet("Saved file ", crayon::green(basename(save_path2)), " to:",
                    bullet = "info", bullet_col = "blue")
    cli::cat_bullet(crayon::cyan(save_path), bullet = "pointer", bullet_col = "yellow")
    cli::cat_line()

  }

  invisible(save_path)
}

