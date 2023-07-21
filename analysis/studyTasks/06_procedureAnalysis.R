# 06_procedureAnalysis.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Procedure Analysis
# Author: Martin Lavallee
# Date: [Add Date]
# Description: The purpose of this script is to.....

# B. Dependencies ----------------------

## include R libraries
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_utilities.R")
source("analysis/private/_procedureAnalysis.R")

# C. Connection ----------------------

# set connection Block
configBlock <- "[Add config block]"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

# connect to database
con <- DatabaseConnector::connect(connectionDetails)
withr::defer(expr = DatabaseConnector::disconnect(con), envir = parent.frame())  #close on exit


# D. Variables -----------------------

### Execution Settings
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

### Analysis Settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/procedureAnalysis.yml"))

# E. Script --------------------

runProcedureAnalysis(con = con,
                     executionSettings = executionSettings,
                     analysisSettings = analysisSettings)

# F. Session Info ------------------------
withr::deferred_run()
