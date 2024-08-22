# A. File Info -----------------------

# Name: Incidence Analysis
# Description: The purpose of this script is to run incidence analyses


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(IncidencePrevalence)
library(CDMConnector)
source("analysis/private/_utilities.R")
source("analysis/private/_incidenceAnalysis.R")


# C. Connection ----------------------

### Set connection Block
# <<<
configBlock <- "[block]"
# >>>

### Provide connection details
executionSettings <- config::get(config = configBlock)

### Connect to server and database
conCdm <- cdmFromConAllDbs(executionSettings = executionSettings)


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("user", "password", "connectionString"))

### Load analysis settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/incidenceAnalysis.yml"))


# E. Script --------------------

### Incidence Analyses

executeIncidenceAnalysis(cdm = conCdm$cdm,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings)


# F. Session Info ------------------------

DBI::dbDisconnect(conCdm$con)
