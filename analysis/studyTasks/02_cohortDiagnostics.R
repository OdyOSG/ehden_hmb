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
configBlock <- "odysseus"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("user", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

#connect to database
con <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(con)) #close on exit

#######if in snowflake uncomment this line#################
#startSnowflakeSession(con, executionSettings)

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

name <- executionSettings$cohortTable

cohortTableNames <- list(cohortTable = paste0(name),
                         cohortInclusionTable = paste0(name, "_inclusion"),
                         cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                         cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                         cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                         cohortCensorStatsTable = paste0(name, "_censor_stats"))


CohortDiagnostics::executeDiagnostics(
  cohortDefinitionSet =  diagCohorts,
  exportFolder = outputFolder,
  cohortTableNames = cohortTableNames,
  cohortDatabaseSchema = executionSettings$workSchema,
  cdmDatabaseSchema = executionSettings$cdmSchema,
  vocabularyDatabaseSchema = executionSettings$vocabSchema,
  databaseId = executionSettings$databaseName,
  connection = con,
  incremental = TRUE,
  minCellCount = 5
)

# F. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
