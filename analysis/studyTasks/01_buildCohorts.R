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
) #close on exit



# D. Study Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results/01_buildCohorts") %>%
  fs::path(executionSettings$databaseName) %>%
  fs::dir_create()



### Add study variables or load from settings

#debug(getCohortManifest)
cohortManifest <- getCohortManifest()


# E. Script --------------------

#######if BAYER uncomment this line#################
#debug(startSnowflakeSession)
startSnowflakeSession(con, executionSettings)


### RUN ONCE - Initialize COhort table #########
#debug(initializeCohortTables)
initializeCohortTables(executionSettings = executionSettings, con = con)


# Generate cohorts
debug(generateCohorts)
generatedCohorts <- generateCohorts(
  executionSettings = executionSettings,
  con = con,
  cohortManifest = cohortManifest,
  outputFolder = outputFolder
)


# F. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
withr::deferred_run()
