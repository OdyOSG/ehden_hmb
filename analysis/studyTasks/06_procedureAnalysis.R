# A. File Info -----------------------

# Name: Procedure Analysis
# Description: The purpose of this script is to run procedure analysis which includes prevalence of procedure and time to initial treatment


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_utilities.R")
source("analysis/private/_procedureAnalysis.R")


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


# D. Variables -----------------------

### Execution Settings
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

### Load analysis settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/procedureAnalysis.yml"))


# E. Script --------------------

startSnowflakeSession(con = con, executionSettings = executionSettings)

executeProcedureAnalysis(con = con,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings)


# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
