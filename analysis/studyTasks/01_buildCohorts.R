# A. Meta Info -----------------------

# Name: Build Cohorts
# Author: Martin Lavallee
# Date: 2023-06-15
# Description: The purpose of 01_buildCohorts.R is to build the HMB cohorts needed
# for the analysis.

# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_buildCohorts.R")
source("analysis/private/_executionSettings.R")

# C. Connection ----------------------

# set connection Block
configBlock <- "[Add config block]"

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

outputFolder <- here::here("results/01_buildCohorts") %>%
  fs::path(executionSettings$databaseName) %>%
  fs::dir_create()

### Add study variables or load from settings

cohortManifest <- getCohortManifest()

## Initialize COhort table
### RUN ONCE #########

#initializeCohortTables(executionSettings = executionSettings, con = con)

# E. Script --------------------

# Generate cohorts
generatedCohorts <- generateCohorts(
  executionSettings = executionSettings,
  con = con,
  cohortManifest = cohortManifest,
  outputFolder = outputFolder
)


# F. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
