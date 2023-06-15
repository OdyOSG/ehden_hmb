# A. Meta Info -----------------------

# Task: Build Cohorts
# Author: [Add Name of Author]
# Date: 2023-04-12
# Description: The purpose of the _buildCohorts.R script is to
# build cohort functions

# B. Functions ------------------------

getCohortManifest <- function(inputPath = here::here("cohortsToCreate")) {

  #get cohort file paths
  cohortFiles <- fs::dir_ls(inputPath, recurse = TRUE, type = "file")
  #get cohort names
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()
  #get cohort type
  cohortType <- fs::path_dir(cohortFiles) %>%
    basename() %>%
    gsub(".*_", "", .)

  #future addition of hash
  hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
    purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
    unname()

  #return tibble with info
  tb <- tibble::tibble(
    name = cohortNames,
    type = cohortType,
    hash = hash,
    file = cohortFiles %>% as.character()
  ) %>%
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )
  return(tb)
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

 if (con@dbms == "snowflake") {

    workSchema <- paste(executionSettings$workDatabase, executionSettings$workSchema, sep = ".")
    cdmSchema <- paste(executionSettings$cdmDatabase, executionSettings$cdmSchema, sep = ".")
    vocabularySchema <- paste(executionSettings$cdmDatabase, executionSettings$vocabSchema, sep = ".")

  } else {

    workSchema <- executionSettings$workSchema
    cdmSchema <- executionSettings$cdmSchema
    vocabularySchema <- executionSettings$vocabSchema
  }

  # prep cohorts for generator
  cohortsToCreate <- prepManifestForCohortGenerator(cohortManifest)

  #path for incremental
  incrementalFolder <- fs::path(outputFolder, executionSettings$databaseName)


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
    cdmDatabaseSchema = executionSettings$cdmSchema,
    cohortDatabaseSchema = workSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate,
    incremental = TRUE,
    incrementalFolder = incrementalFolder
  )

  #get cohort counts
  cohortCounts <- CohortGenerator::getCohortCounts(
    connection = con,
    cohortDatabaseSchema = workSchema,
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

  savePath <- fs::path(outputFolder, executionSettings$databaseId, "cohortManifest.csv")
  readr::write_csv(x = tb, file = savePath)
  cli::cat_bullet("Saving Generated Cohorts to ", crayon::cyan(savePath),
                  bullet = "tick", bullet_col = "green")

  return(cohortCounts)

}
