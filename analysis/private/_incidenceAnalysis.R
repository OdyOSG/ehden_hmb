# A. Meta Info -----------------------

# Task: Incidence
# Author: Martin Lavallee
# Date: 2023-07-26
# Description: The purpose of the _incidence.R script is to
# provide functions for the incidence analysis portion of the study

# B. Functions ------------------------

defineIncidenceAnalysis <- function(cohortId,
                                    cohortName,
                                    denomCohorts,
                                    irSettings) {

  targets <- purrr::pmap(
    denomCohorts,
    ~CohortIncidence::createCohortRef(
      id = ..2,
      name = ..1
    )
  )
  o1 <- CohortIncidence::createOutcomeDef(
    id = cohortId,
    name = cohortName,
    cohortId = cohortId,
    cleanWindow = irSettings$cleanWindow
  )


  timeMap <- tibble::tibble(
    id = seq_along(irSettings$startOffset),
    startDays = irSettings$startOffset,
    endDays = irSettings$endOffset
  )

  tars <- purrr::pmap(
    timeMap,
    ~CohortIncidence::createTimeAtRiskDef(
      id = ..1,
      startWith = irSettings$startWith,
      startOffset = ..2,
      endWith = irSettings$endsWith,
      endOffset = ..3
    )
  )

  # make all permutations of denominotor pop and tar
  analysisMap <- tidyr::expand_grid(
    't' = purrr::map_int(targets, ~.x$id), # this needs to be the cohort Id of the denom cohort to be used
    'tar' = seq_along(tars)
  )



  analysisList <- purrr::pmap(
    analysisMap,
    ~CohortIncidence::createIncidenceAnalysis(
      targets = ..1,
      outcomes = c(o1$id),
      tars = ..2
    )
  )

  byYearStrata <- CohortIncidence::createStrataSettings(byYear = TRUE)

  irDesign <- CohortIncidence::createIncidenceDesign(
    targetDefs = targets,
    outcomeDefs = list(o1),
    tars = tars,
    analysisList = analysisList,
    # add year strata
    strataSettings = byYearStrata
  )

  return(irDesign)

}

generateIncidenceAnalysis <- function(con,
                                      executionSettings,
                                      cohortId,
                                      cohortName,
                                      denomCohorts,
                                      irSettings,
                                      refId) {


  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- paste(executionSettings$workDatabaseSchema, executionSettings$cohortTable, sep = ".")
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  timeMap <- tibble::tibble(
    id = seq_along(irSettings$startOffset),
    startDays = irSettings$startOffset,
    endDays = irSettings$endOffset
  )

  irDesign <- defineIncidenceAnalysis(cohortId = cohortId,
                                      cohortName = cohortName,
                                      denomCohorts = denomCohorts,
                                      irSettings = irSettings)


  buildOptions <- CohortIncidence::buildOptions(
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    sourceName = databaseId,
    resultsDatabaseSchema = workDatabaseSchema,
    vocabularySchema = cdmDatabaseSchema,
    useTempTables = FALSE,
    refId = refId)

  cli::cat_line()
  cli::cat_bullet("Executing Incidence Analysis Id: ", crayon::green(refId),
                  bullet = "checkbox_on", bullet_col = "green")
  cli::cat_bullet("Denominator Population", bullet = "pointer", bullet_col = "yellow")
  cli::cat_line("   ", crayon::yellow(cli::symbol$star), " Cohort Id: ", crayon::green(cohortId), "\n",
                "   ", crayon::yellow(cli::symbol$star)," Cohort Name: ", crayon::green(cohortName))

  cli::cat_bullet("Outcome Population", bullet = "pointer", bullet_col = "yellow")
  print_outcomeIds <- paste(denomCohorts$id, collapse = ", ")
  cli::cat_line("   ", crayon::yellow(cli::symbol$star), " Outcomes Id: ", crayon::green(print_outcomeIds))
  cli::cat_bullet("Time at Risk", bullet = "pointer", bullet_col = "yellow")
  print_tar <- paste(timeMap$startDays, timeMap$endDays, sep = "-")
  cli::cat_line("   ", crayon::yellow(cli::symbol$star), " tar: ", crayon::green(print_tar))


  executeResults <- CohortIncidence::executeAnalysis(
    connection = con,
    incidenceDesign = irDesign,
    buildOptions = buildOptions)


  verboseSave(
    object = executeResults,
    saveName = paste("incidence_analysis_ref", refId, sep = "_"),
    saveLocation = outputFolder
  )

  invisible(executeResults)

}


executeIncidenceAnalysis <- function(con,
                                     executionSettings,
                                     analysisSettings) {


  ## get cohort Ids
  targetCohortId <- analysisSettings$incidenceAnalysis$cohorts$targetCohort$id
  targetCohortName <- analysisSettings$incidenceAnalysis$cohorts$targetCohort$name
  denomCohorts <- analysisSettings$incidenceAnalysis$cohorts$denominatorCohort
  irSettings <- analysisSettings$incidenceAnalysis$incidenceSettings

  cli::cat_boxx("Building Incidence Analysis")
  cli::cat_line()

  tik <- Sys.time()
  for (i in seq_along(targetCohortId)) {

    generateIncidenceAnalysis(
      con = con,
      executionSettings = executionSettings,
      cohortId = targetCohortId[i],
      cohortName = targetCohortName[i],
      denomCohorts = denomCohorts,
      irSettings = irSettings,
      refId = i
    )
  }
  tok <- Sys.time()
  cli::cat_bullet("Execution Completed at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")

  invisible(denomCohorts)

}

