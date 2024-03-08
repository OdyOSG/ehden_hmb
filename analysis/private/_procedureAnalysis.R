# A. File Info -----------------------

# Task: Procedure Analysis
# Description: The purpose of the _procedureAnalysis.R script is to provide underlying functions to run the procedure analysis for EHDEN HMB study.

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


getTteRes <- function(tb, outcomeCohortId) {

  ##CORRECTION needs to be time to event happening
  tab2 <- tb %>%
    dplyr::filter(cohortDefinitionId == outcomeCohortId) %>%
    dplyr::mutate(
      event = ifelse(is.na(cohortDefinitionId), 0, 1),
      duration = dplyr::case_when(
        event == 0 ~ as.integer(cohortEndDate - cohortStartDate) / 365.25,
        event == 1 ~ as.integer(eventDate - cohortStartDate) / 365.25
      )
    ) %>%
    dplyr::select(duration, event) %>%
    dplyr::filter(duration <= 3)


  if (nrow(tab2) > 0) {

    # get surv fit object
    survFit <- ggsurvfit::survfit2(
      survival::Surv(duration, event) ~ 1, data = tab2
    )

    # retrieve tidy survfit
    survDat <- ggsurvfit::tidy_survfit(survFit) %>%
      dplyr::select(time, `n.risk`, `n.event`, estimate, std.error) %>%
      # dplyr::filter(
      #   time <= 3 # only take first 3 years of data
      # ) %>%
      dplyr::mutate(
        outcomeCohortId = !!outcomeCohortId
      )
  } else {
    # if no data return an all zero row
    survDat <- tibble::tibble(
      time = 0,
      n.risk = 0,
      n.event = 0,
      estimate = 0,
      std.error = 0,
      outcomeCohortId = outcomeCohortId
    )
  }

  return(survDat)
}


getTteResKM <- function(tb, outcomeCohortId) {

  ##CORRECTION needs to be time to event happening
  tab2 <- tb %>%
    dplyr::filter(cohortDefinitionId %in% outcomeCohortId$id) %>%
    dplyr::mutate(
      event = ifelse(is.na(cohortDefinitionId), 0, 1),
      duration = dplyr::case_when(
        event == 0 ~ as.integer(cohortEndDate - cohortStartDate) / 365.25,
        event == 1 ~ as.integer(eventDate - cohortStartDate) / 365.25
      )
    ) %>%
    dplyr::left_join(outcomeCohortId, by = c("cohortDefinitionId" = "id")) %>%
    dplyr::select(duration, event, cohortDefinitionId, name) %>%
    dplyr::filter(duration <= 3)


  if (nrow(tab2) > 0) {

    # get surv fit object
    survFit <- ggsurvfit::survfit2(
      survival::Surv(duration, event) ~ name, data = tab2
    )

  }

  return(survFit)
}


executeProcedureAnalysis <- function(con,
                                 executionSettings,
                                 analysisSettings) {

  # Step 0: Prep -----------

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  ## get cohort Ids
  targetCohortIds <- analysisSettings$procedureAnalysis$cohorts$targetCohort$id
  procedureCohort <- analysisSettings$procedureAnalysis$cohorts$procCohorts

  ## get time windows
  timeA <- analysisSettings$procedureAnalysis$timeWindow$startDay
  timeB <- analysisSettings$procedureAnalysis$timeWindow$endDay

  # get outputFolder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  # get procPrevFolder
  procPrevFolder <- outputFolder[1]
  # get time to intervention folder
  ttiFolder <- outputFolder[2]

  txt <- glue::glue("Running Procedure Analysis for {crayon::yellow(databaseId)}")
  cli::cat_rule(txt)

  for (i in seq_along(targetCohortIds)) {

    idx <- targetCohortIds[i]

    txt <- glue::glue("Running procedure analysis for cohort id {crayon::magenta(idx)}")
    cli::cat_bullet(txt, bullet = "pointer", bullet_col = "yellow")


    # Step 1: Get procedure table ------------
    cli::cat_line()
    cli::cat_bullet("Step 1: Getting analysis table from db",
                    bullet = "checkbox_on", bullet_col = "green")

    tb <- get_procedure_table(
      con,
      workDatabaseSchema = workDatabaseSchema,
      cohortTable = cohortTable,
      targetCohortId = idx,
      outcomeCohortIds = procedureCohort$id
    )

    # Step 2: Run Procedure prevalence ------------
    cli::cat_line()
    cli::cat_bullet("Step 2: Calculating prevalence of procedures",
                    bullet = "checkbox_on", bullet_col = "green")

    # find number of persons
    num_persons <- length(unique(tb$subjectId))

    # count events for procedures in all time
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

    # prevalence in specified time window
    tab1b <- purrr::map2_dfr(
      timeA,
      timeB,
      ~getWindowPrevalence(tb = tb, timeA = .x, timeB = .y)
    )

    tab1 <- dplyr::bind_rows(tab1a, tab1b)

    fileNm <- glue::glue("procedure_prevalence_{idx}")

    #save prevalence object to folder
    verboseSave(
      object = tab1,
      saveName = fileNm,
      saveLocation = procPrevFolder
    )

    # Step 3: Run Time to event -----------
    cli::cat_line()
    cli::cat_bullet("Step 3: Calculating time to intervention for procedures",
                    bullet = "checkbox_on", bullet_col = "green")


    # subset table for survival analysis (table)
    survDat <- purrr::map_dfr(
      procedureCohort$id,
      ~getTteRes(tb, outcomeCohortId = .x)
    )

    # subset table for survival analysis (survfit)
    survDatKM <- getTteResKM(tb = tb, outcomeCohortId = procedureCohort)

    # Export time to event tables (survfit)
    readr::write_rds(survDatKM,
                     here::here(ttiFolder, paste0("tti_", databaseId, "_", targetCohortIds[i], ".rds"))
                     )

    fileNm2 <- glue::glue("procedure_survival_{idx}")

    # Export time to event tables (csv)
    verboseSave(
      object = survDat,
      saveName = fileNm2,
      saveLocation = ttiFolder
    )

  } #iterate to strata

  invisible(tb)
}
