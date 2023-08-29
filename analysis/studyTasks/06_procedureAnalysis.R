# 06_procedureAnalysis.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Procedure Analysis
# Author: Martin Lavallee
# Date: 08/29/2023
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


# D. Variables -----------------------

### Execution Settings
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

### Analysis Settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/procedureAnalysis.yml"))

# E. Script --------------------

executeProcedureAnalysis(con = con,
                     executionSettings = executionSettings,
                     analysisSettings = analysisSettings)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
