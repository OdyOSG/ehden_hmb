# A. File Info -----------------------

# Name: Treatment Patterns
# Description: The purpose of this script is to run the treatment patterns module


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_utilities.R")
source("analysis/private/_treatmentPatterns.R")
source("analysis/private/_treatmentHistory_helpers.R")
source("analysis/private/_treatmentHistory.R")


# C. Connection ----------------------

# set connection Block
# <<<
configBlock <- "[block]"
# >>>

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms",config = configBlock),
  user = config::get("user",config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

### Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

### Load analysis settings
analysisSettings1 <- readSettingsFile(here::here("analysis/settings/postIndexPrevalence1.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/treatmentPatterns1.yml"))
analysisSettings3 <- readSettingsFile(here::here("analysis/settings/treatmentPatterns2.yml"))


# E. Script --------------------

### Post index prevalence
executePostIndexDrugUtilization(con = con,
                                executionSettings = executionSettings,
                                analysisSettings = analysisSettings1)

### Treatment history
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings2)

### Treatment patterns
executeTreatmentPatterns(con = con,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings2)

## Time to discontinuation
executeTimeToEvent(con = con,
                   executionSettings = executionSettings,
                   analysisSettings = analysisSettings2)


### Treatment history 2 --- all exposures censor hysterectomy
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings3)

### Treatment patterns --- all exposures censor hysterectomy
executeTreatmentPatterns(con = con,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings3)


# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
