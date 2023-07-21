# A. Meta Info -----------------------

# Task: Procedure Analysis
# Author: Martin Lavallee
# Date: 2023-07-21
# Description: The purpose of the _procedureAnalysis.R script is to
# provide underlying functions to run the procedure analysis for EHDEN HMB study.

source("analysis/private/_utilities.R")

# B. Functions ------------------------

get_procedure_table <- function(con,
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
    WHERE t.cohort_definition_id = @targetId
  ;
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
  cli::cat_bullet("Procedure table build took: ", crayon::red(tok_format),
                  bullet = "pointer", bullet_col = "yellow")

  return(tbl)
}


getWindowPrevalence <- function(tb, timeA, timeB) {

  tb <- tb %>% tibble::as_tibble()

  #prep data frame
  dt <- tb %>%
    dplyr::mutate(
      timeInCohort = as.integer(cohortEndDate - cohortStartDate),
      timeToEvent = as.integer(eventDate - cohortStartDate),
      inWindow = dplyr::if_else(timeInCohort >= timeA, 1, 0, 0)
    )

  #find denominator
  numInWindow <- dt %>%
    dplyr::filter(inWindow == 1) %>%
    dplyr::distinct(subjectId) %>%
    dplyr::count() %>%
    dplyr::pull()

  #summarize prevalence
  dt2 <- dt %>%
    dplyr::mutate(
      eventInWindow = dplyr::if_else(dplyr::between(timeToEvent, timeA, timeB), 1, 0, 0)
    ) %>%
    dplyr::group_by(cohortDefinitionId) %>%
    dplyr::summarize(
      numEvents = sum(eventInWindow)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(cohortDefinitionId)) %>%
    dplyr::mutate(
      window = glue::glue("{timeA} - {timeB}"),
      pct = numEvents / numInWindow
    ) %>%
    dplyr::select(window, cohortDefinitionId, numEvents, pct)

  return(dt2)
}


runProcedureAnalysis <- function(con,
                                 executionSettings,
                                 analysisSettings) {


  # Step 0: Prep -----------

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  ## get cohort Ids
  targetCohortIds <- analysisSettings$procedureAnalysis$cohorts$targetCohortId$id
  procedureCohortIds <- analysisSettings$procedureAnalysis$cohorts$procCohortIds$id

  ## get time windows
  timeA <- analysisSettings$procedureAnalysis$prevalenceTimeWindow$startDays
  timeB <- analysisSettings$procedureAnalysis$prevalenceTimeWindow$endDays

  # get outputFolder
  outputFolder <- fs::path(here::here("results"), analysisSettings$procedureAnalysis$outputFolder) %>%
    fs::dir_create()

  txt <- glue::glue("Running Procedure Analysis for {crayon::yellow(databaseId)}")
  cli::cat_rule(txt)

  # Step 1: Get procedure table ------------
  cli::cat_line()
  cli::cat_bullet("Producing analysis table for procedures from dbms",
                  bullet = "checkbox_on", bullet_col = "green")


  tb <- get_procedure_table(
    con,
    workDatabaseSchema = workDatabaseSchema,
    cohortTable = cohortTable,
    targetCohortId = targetCohortIds,
    outcomeCohortIds = procedureCohortIds
  )

  # Step 2: Run Procedure prevalence ------------
  cli::cat_line()
  cli::cat_bullet("Calculating prevalence of procedures",
                  bullet = "checkbox_on", bullet_col = "green")

  num_persons <- length(unique(tb$subjectId))
  tab1a <- tb %>%
    dplyr::filter(!is.na(cohortDefinitionId)) %>%
    dplyr::group_by(cohortDefinitionId) %>%
    dplyr::count(name = "numEvents") %>%
    dplyr::ungroup() %>%
    mutate(
      pct = numEvents / num_persons,
      window = "All"
    ) %>%
    dplyr::select(window, cohortDefinitionId, numEvents, pct)

  tab1b <- purrr::map2_dfr(
    timeA,
    timeB,
    ~getWindowPrevalence(tb = tb, timeA = .x, timeB = .y)
  )

  tab1 <- dplyr::bind_rows(tab1a, tab1b)

  verboseSave(
    object = tab1,
    saveName = "procedure_prevalence",
    saveLocation = outputFolder
  )


  # Step 3: Run Time to event -----------
  cli::cat_line()
  cli::cat_bullet("Calculating time to intervention for procedures",
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
  survDat <- ggsurvfit::tidy_survfit(survFit) %>%
    dplyr::select(time, `n.risk`, `n.event`, estimate:strata)

  verboseSave(
    object = survDat,
    saveName = "procedure_survival",
    saveLocation = outputFolder
  )

  invisible(tb)

}
