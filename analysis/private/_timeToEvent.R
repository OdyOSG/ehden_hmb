# A. File Info -----------------------

# Task: Time to Event
# Description: The purpose of the _timeToEvent.R script is to.....

source("analysis/private/_utilities.R")


# B. Functions ------------------------

get_tte_table <- function(con,
                          workDatabaseSchema,
                          cohortTable,
                          targetCohortId,
                          outcomeCohortIds) {

  sql <- "
    SELECT
        t.subject_id,
      	t.cohort_start_date,
      	t.cohort_end_date,
      	o.cohort_definition_id,
      	o.cohort_start_date AS event_date
    FROM @workDatabaseSchema.@cohortTable t
    LEFT JOIN @workDatabaseSchema.@cohortTable o
        ON t.subject_id = o.subject_id
      	  AND o.cohort_start_date >= t.cohort_start_date
      	  AND o.cohort_start_date <= t.cohort_end_date
      	  AND o.cohort_definition_id IN (@outcomeId)
    WHERE t.cohort_definition_id = @targetId;
  "

  tteSql <- SqlRender::render(
    sql,
    workDatabaseSchema = workDatabaseSchema,
    cohortTable = cohortTable,
    targetId = targetCohortId,
    outcomeId = outcomeCohortIds
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  tik <- Sys.time()
  tbl <- DatabaseConnector::querySql(con, sql = tteSql, snakeCaseToCamelCase = TRUE)
  tok <- Sys.time()

  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_line()
  cli::cat_bullet("TTE table build took: ", crayon::red(tok_format),
                  bullet = "pointer", bullet_col = "yellow")

  return(tbl)
}


runTimeToEvent <- function(con,
                           executionSettings,
                           analysisSettings) {

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  ## get cohort Ids
  targetCohortIds <- analysisSettings$timeToEvent$cohorts$targetCohortId$id
  targetCohortName <- analysisSettings$timeToEvent$cohorts$targetCohortId$name
  tteCohortsIds <- analysisSettings$timeToEvent$cohorts$tteCohorts$id

  # get outputFolder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings$timeToEvent$outputFolder) %>%
    fs::dir_create()

  txt <- glue::glue("Running Time to event Analysis for {crayon::yellow(databaseId)}")
  cli::cat_rule(txt)

  survDat <- vector('list', length(targetCohortIds))

  for (i in seq_along(targetCohortIds)) {

    tb <- get_tte_table(
      con,
      workDatabaseSchema = workDatabaseSchema,
      cohortTable = cohortTable,
      targetCohortId = targetCohortIds[i],
      outcomeCohortIds = tteCohortsIds
    )

    cli::cat_line()
    cohort_print <- paste(targetCohortIds[i], targetCohortName[i], sep = ": ")
    cli::cat_bullet("Calculating time to event for cohort ", crayon::magenta(cohort_print),
                    bullet = "checkbox_on", bullet_col = "green")

    tab2 <- tb %>%
      dplyr::filter(!is.na(cohortDefinitionId)) %>%
      dplyr::mutate(
        event = 1,
        duration = as.integer(eventDate - cohortStartDate) / 365.25
      ) %>%
      dplyr::select(cohortDefinitionId, duration, event)

    survFit <- ggsurvfit::survfit2(
      survival::Surv(duration, event) ~ cohortDefinitionId, data = tab2
    )
    survDat[[i]] <- ggsurvfit::tidy_survfit(survFit) %>%
      dplyr::select(time, `n.risk`, `n.event`, estimate:strata) %>%
      dplyr::mutate(
        targetCohortId = targetCohortIds[i],
        targetCohortName = targetCohortName[i]
      )
  }

  survDat <- do.call('rbind', survDat)

  verboseSave(
    object = survDat,
    saveName = "tte_analysis",
    saveLocation = outputFolder
  )

}
