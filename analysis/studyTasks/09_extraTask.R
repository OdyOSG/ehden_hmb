# A. File Info -----------------------

# Name: Extra task
# Description: 1) Build target cohort plus strata, 2) CohortDiagnostics csv files, 3) pre and post-index prevalence of hormonalIUDLng


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_utilities.R")
source("analysis/private/_buildCohorts.R")
source("analysis/private/_buildStrata.R")
source("analysis/private/_conceptPrevalence.R")
source("analysis/private/_cohortPrevalence.R")
source("analysis/private/_conditionRollup.R")
source("analysis/private/_treatmentPatterns.R")


# C. Connection ----------------------

## Set connection Block
# <<<
configBlock <- "[block]"
# >>>

## Provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

## Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Study Variables -----------------------

## Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "01_buildCohortsExtra") %>%
  fs::dir_create()

outputFolderCD <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "02_cohortDiagnosticsExtra") %>%
  fs::dir_create()

## Add study variables or load from settings
cohortManifest <- getCohortManifest(inputPath = here::here("cohortsToCreateExtra"))

## Load analysis settings
analysisSettingsStrata <- readSettingsFile(here::here("analysis/settings/strataExtra.yml"))
analysisSettingsPre <- readSettingsFile(here::here("analysis/settings/baselineCharacteristicsExtra.yml"))
analysisSettingsPost <- readSettingsFile(here::here("analysis/settings/postIndexPrevalenceExtra.yml"))


# E. Script --------------------

dbType <- config::get("dbms", config = configBlock)

if (dbType == "snowflake") {

  startSnowflakeSession(con = con, executionSettings = executionSettings)

}


## 1) Build Cohorts --------------------

## Initialize cohort tables
initializeCohortTables(executionSettings = executionSettings, con = con, dropTables = TRUE)

## Generate cohorts
generatedCohorts <- generateCohorts(
  executionSettings = executionSettings,
  con = con,
  cohortManifest = cohortManifest,
  outputFolder = outputFolder
)


## Build strata
buildStrata(con = con,
            executionSettings = executionSettings,
            analysisSettings = analysisSettingsStrata)


## 2) Cohort Diagnostics --------------------

diagCohorts <- cohortManifest %>%
  dplyr::filter(type == "target")

### Run cohort diagnostics
runCohortDiagnosticsExtra(executionSettings = executionSettings,
                          con = con,
                          cohortManifest = diagCohorts,
                          outputFolder = outputFolderCD)


## 3) Pre/post-index characterization --------------------

## Cohort characterization - Pre
executeCohortPrevalence(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettingsPre)

## Cohort characterization - Post
executePostIndexDrugUtilization(con = con,
                                executionSettings = executionSettings,
                                analysisSettings = analysisSettingsPost)

## 4) Zip results --------------------

zipResultsExtra(database = configBlock)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)

