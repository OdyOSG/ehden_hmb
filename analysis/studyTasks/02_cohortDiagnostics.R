# A. File Info -----------------------

# Name: Cohort Diagnostics
# Description: The purpose of 02_cohortDiagnostics.R is to run cohort diagnostics for the hmb cohorts


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_buildCohorts.R")
source("analysis/private/_utilities.R")


# C. Connection ----------------------

### Set connection Block
# <<<
configBlock <- "[block]"
# >>>

### Provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

### Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Study Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at( c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "02_cohortDiagnostics") %>%
  fs::dir_create()

### Add study variables or load from settings
diagCohorts <- getCohortManifest() %>%
  dplyr::filter(
    type == "target"
  )


# E. Script --------------------

dbType <- config::get("dbms", config = configBlock)

if (dbType == "snowflake") {

  startSnowflakeSession(con = con, executionSettings = executionSettings)

}

### Run cohort diagnostics
runCohortDiagnostics(executionSettings = executionSettings,
                     con = con,
                     cohortManifest = diagCohorts,
                     outputFolder = outputFolder)


# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
