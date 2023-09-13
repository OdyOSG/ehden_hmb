# A. Meta Info -----------------------

# Task: Treatment History
# Author: Carina
# Date: 2023-07-26
# Description: The purpose of the _treatmentHistory.R script is to.....

# B. Functions ------------------------

get_tx_history <- function(con,
                           workDatabaseSchema,
                           cohortTable,
                           targetId,
                           targetName,
                           treatmentCohorts,
                           thSettings,
                           outputFolder) {



  #collect cohorts
  current_cohorts <- collect_cohorts(con = con,
                                     workDatabaseSchema = workDatabaseSchema,
                                     cohortTable = cohortTable,
                                     targetId = targetId,
                                     eventIds = treatmentCohorts$id)



  # Run treatment history
  tik <- Sys.time()
  cli::cat_line(crayon::blue("Do Treatment History"))
  res <- doCreateTreatmentHistory(current_cohorts,
                                  targetCohortId = targetId,
                                  eventCohortIds = treatmentCohorts$id,
                                  periodPriorToIndex = thSettings$periodPriorToIndex,
                                  includeTreatments = thSettings$includeTreatments) %>%
    doEraDuration(minEraDuration = thSettings$minEraDuration) %>%
    doEraCollapse(eraCollapseSize = thSettings$eraCollapseSize) %>%
    doCombinationWindow(combinationWindow = thSettings$combinationWindow,
                        minPostCombinationDuration = thSettings$minPostCombinationDuration) %>%
    doFilterTreatments(filterTreatments = thSettings$filterTreatments) %>%
    postProcess(eventCohortIds = treatmentCohorts$id,
                eventCohortNames = treatmentCohorts$name,
                maxPathLength = thSettings$maxPathLength)
  res$duration_era <- as.integer(res$duration_era)

  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_line("\nTreatment History built at: ", tok)
  cli::cat_line("\nTreatment History build took: ", tok_format)

  save_name <- paste("th", targetId,  sep = "_")
  save_path <- fs::path(outputFolder, save_name, ext = "parquet")
  arrow::write_parquet(x = res, sink = save_path)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(save_path)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(outputFolder), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  invisible(res)

}

## Run bulk treatment history module -------------

runTreatmentHistory <- function(con,
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

  thHistoryFolder <- outputFolder[[1]] # changed based on settings

  targetCohorts <- analysisSettings$treatmentPatterns$cohorts$targetCohort
  thSettings <- analysisSettings$treatmentPatterns$treatmentHistorySettings
  treatmentCohorts <- analysisSettings$treatmentPatterns$cohorts$txCohorts

  cli::cat_boxx(crayon::magenta("Begin Building Treatment History"))
  tik <- Sys.time()

  for (i in seq_along(targetCohorts$id)) {

        # subset the targetId
        tmp_targetId <- targetCohorts$id[i]
        tmp_targetName <- targetCohorts$name[i]

        # subset treatmentCohorts
        txCohorts <- treatmentCohorts %>%
          dplyr::select(id, name) %>%
          dplyr::mutate(type = "event")

        # specify save path
        save_path <- fs::path(thHistoryFolder) %>%
          fs::dir_create()

        # Print statements to orient run
        cli::cat_rule()
        txt1 <- paste0(targetCohorts$name[i], " (id:", targetCohorts$id[i], ")")
        cli::cat_bullet(crayon::green("Target Cohort: "), txt1, bullet = "pointer", bullet_col = "yellow")
        # cli::cat_bullet(crayon::green("Drug Class: "), drugClass[j], bullet = "pointer", bullet_col = "yellow")
        # cli::cat_bullet(crayon::green("Era Collapse Size: "), eraCollapseSize[k], bullet = "pointer", bullet_col = "yellow")
        txt2 <- paste(txCohorts$name, collapse = ", ")
        cli::cat_bullet(crayon::green("Event Cohorts: "), txt2, bullet = "pointer", bullet_col = "yellow")
        cli::cat_line()


        #run tx history
        get_tx_history(con = con,
                       workDatabaseSchema = workDatabaseSchema,
                       cohortTable = cohortTable,
                       targetId = tmp_targetId,
                       targetName = tmp_targetName,
                       treatmentCohorts = txCohorts,
                       thSettings = thSettings,
                       outputFolder = save_path)



  }

  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")

  invisible(treatmentCohorts)

}


