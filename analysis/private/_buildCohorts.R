# A. File Info -----------------------

# Task: Build Cohorts
# Description: The purpose of the _buildCohorts.R script is to build cohort functions


# B. Functions ------------------------

initializeCohortTables <- function(executionSettings,
                                   con,
                                   dropTables = FALSE) {

  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  ## Drop cohort tables
  if (dropTables == TRUE) {

    ## Delete csv files from "01_buildCohorts" folder
    manifestPath <- here::here("results", executionSettings$databaseName, "01_buildCohorts")
    pathFiles <- list.files(manifestPath,  full.names = TRUE)
    sapply(pathFiles, unlink)


    ## Drop cohort tables
    for (i in 1:length(cohortTableNames)) {

      sql <- "DROP TABLE IF EXISTS @writeSchema.@tableName;"

      dropSql <- SqlRender::render(
        sql,
        writeSchema = executionSettings$workDatabaseSchema,
        tableName = cohortTableNames[i]
      ) %>%
        SqlRender::translate(targetDialect = "snowflake")

      DatabaseConnector::executeSql(connection = con, dropSql, progressBar = FALSE)

    }
  }

  ## Create cohort tables
  CohortGenerator::createCohortTables(connection = con,
                                      cohortDatabaseSchema = executionSettings$workDatabaseSchema,
                                      cohortTableNames = cohortTableNames,
                                      incremental = TRUE)
  invisible(cohortTableNames)
}



prepManifestForCohortGenerator <- function(cohortManifest) {

  cohortsToCreate <- cohortManifest %>%
    dplyr::mutate(
      json = purrr::map_chr(file, ~readr::read_file(.x))
    ) %>%
    dplyr::select(id, name, json) %>%
    dplyr::rename(cohortId = id, cohortName = name)

  cohortsToCreate$sql <- purrr::map_chr(
    cohortsToCreate$json,
    ~CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(.x),
                              CirceR::createGenerateOptions(generateStats = TRUE)))

  return(cohortsToCreate)
}


generateCohorts <- function(executionSettings,
                            con,
                            cohortManifest,
                            outputFolder,
                            type = "analysis") {


  # prep cohorts for generator
  cohortsToCreate <- prepManifestForCohortGenerator(cohortManifest)

  #path for incremental
  incrementalFolder <- fs::path(outputFolder)


  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))

  #generate cohorts
  CohortGenerator::generateCohortSet(
    connection = con,
    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema =  executionSettings$workDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate,
    incremental = TRUE,
    incrementalFolder = incrementalFolder
  )

  #get cohort counts
  cohortCounts <- CohortGenerator::getCohortCounts(
    connection = con,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    cohortDefinitionSet = cohortsToCreate
  ) %>%
    dplyr::select(cohortId, cohortName, cohortEntries, cohortSubjects)

  # save generated cohorts
  tb <- cohortManifest %>%
    dplyr::left_join(cohortCounts %>%
                       dplyr::select(cohortId, cohortEntries, cohortSubjects),
                     by = c("id" = "cohortId")) %>%
    dplyr::rename(
      entries = cohortEntries,
      subjects = cohortSubjects) %>%
    dplyr::select(
      id, name, type, entries, subjects, file
    )

  savePath <- fs::path(outputFolder, "cohortManifest.csv")
  readr::write_csv(x = tb, file = savePath)
  cli::cat_bullet("Saving Generated Cohorts to ", crayon::cyan(savePath),
                  bullet = "tick", bullet_col = "green")

  return(cohortCounts)
}

# Run Cohort Diagnostis
# Description: this function is used to run cohort diagnostics. preps cohorts for run
# and then executes cohort diagnostics
# inputs:
# con -> the connection objection accessing the dbms storing the omop data
# executionSettings -> the DatabaseSchema information required to run the study
# cohortManifest -> the set of cohorts used to run diagnostics
# outputFolder -> the save location of the cohort diagnostics run
# return:
# invisible return of the cohortsToRun for diagnostics. The output of this function
# is a folder with cohort diagnostics run.

runCohortDiagnostics <- function(con,
                                 executionSettings,
                                 cohortManifest,
                                 outputFolder) {

  cohortsToRun <- prepManifestForCohortGenerator(cohortManifest) %>%
    dplyr::mutate(
      cohortId = as.numeric(cohortId)
    )

  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  #Run cohort diagnostics
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = cohortsToRun,
    exportFolder = outputFolder,
    cohortTableNames = cohortTableNames,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
    vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema,
    databaseId = executionSettings$databaseName,
    connection = con,
    incremental = TRUE,
    minCellCount = 5
  )

  cli::cat_bullet("Saving Cohort Diagnostics to ", crayon::cyan(outputFolder),
                  bullet = "tick", bullet_col = "green")

  invisible(cohortsToRun)
}


runCohortDiagnosticsExtra <- function(con,
                                      executionSettings,
                                      cohortManifest,
                                      outputFolder) {

  cohortsToRun <- prepManifestForCohortGenerator(cohortManifest) %>%
    dplyr::mutate(
      cohortId = as.numeric(cohortId)
    )

  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  # Run cohort diagnostics
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = cohortsToRun,
    exportFolder = outputFolder,
    cohortTableNames = cohortTableNames,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
    vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema,
    databaseId = executionSettings$databaseName,
    runInclusionStatistics = TRUE,
    runIncludedSourceConcepts = FALSE,
    runOrphanConcepts = FALSE,
    runTimeSeries = FALSE,
    runVisitContext = FALSE,
    runBreakdownIndexEvents = FALSE,
    runIncidenceRate = FALSE,
    runCohortRelationship = FALSE,
    runTemporalCohortCharacterization = FALSE,
    connection = con,
    incremental = TRUE,
    minCellCount = 5
  )

  cli::cat_bullet("Saving Cohort Diagnostics to ", crayon::cyan(outputFolder),
                  bullet = "tick", bullet_col = "green")

  invisible(cohortsToRun)
}
