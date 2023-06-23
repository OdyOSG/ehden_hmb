# A. Meta Info -----------------------

# Name: Cohort Diagnostics
# Author: Martin Lavallee
# Date: 2023-06-15
# Description: The purpose of 02_cohortDiagnostics.R is to run cohort diagnostics
# for the hmb cohort.

# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_buildCohorts.R")
source("analysis/private/_executionSettings.R")

# C. Connection ----------------------

# set connection Block
configBlock <- "optum"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

#connect to database
con <- DatabaseConnector::connect(connectionDetails)
withr::defer(
  expr = DatabaseConnector::disconnect(con),
  envir = parent.frame()
) # close on exit



# D. Study Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at( c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results/02_cohortDiagnostics") %>%
  fs::path(executionSettings$databaseName) %>%
  fs::dir_create()

### Add study variables or load from settings
diagCohorts <- getCohortManifest()
# E. Script --------------------


#######if BAYER uncomment this line#################
startSnowflakeSession(con, executionSettings)


# Create cohort table names
name <- executionSettings$cohortTable

cohortTableNames <- list(cohortTable = paste0(name),
                         cohortInclusionTable = paste0(name, "_inclusion"),
                         cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                         cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                         cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                         cohortCensorStatsTable = paste0(name, "_censor_stats"))

 #Run cohort diagnostics
CohortDiagnostics::executeDiagnostics(
  cohortDefinitionSet =  diagCohorts,
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

# F. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
withr::deferred_run()
