# A. File Info -----------------------

# Name: Treatment Patterns
# Description: The purpose of this script is to run the treatment patterns module


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(ggsurvfit)
source("analysis/private/_utilities.R")
source("analysis/private/_treatmentPatterns.R")
source("analysis/private/_treatmentHistory_helpers.R")
source("analysis/private/_treatmentHistory.R")


# C. Connection ----------------------

## Set connection Block
# <<<
configBlock <- "cprdAurum"
# >>>

## Provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms",config = configBlock),
  user = config::get("user",config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

## Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Variables -----------------------

## Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

## Load analysis settings
analysisSettings1 <- readSettingsFile(here::here("analysis/settings/postIndexPrevalence.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/treatmentPatternsAnalysis1.yml"))
analysisSettings3 <- readSettingsFile(here::here("analysis/settings/treatmentPatternsAnalysis2.yml"))


# E. Script --------------------

startSnowflakeSession(con = con, executionSettings = executionSettings)

# ## Post index prevalence
# executePostIndexDrugUtilization(con = con,
#                                 executionSettings = executionSettings,
#                                 analysisSettings = analysisSettings1)
#
# ### Without NSAIDS --------------------
# ## Treatment history
#
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings2)

## Treatment patterns
executeTreatmentPatterns(con = con,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings2)
#
# ## Time to discontinuation
# executeTimeToEvent(con = con,
#                    executionSettings = executionSettings,
#                    analysisSettings = analysisSettings2)
#
#
### With NSAIDS (Sensitivity analysis) --------------------
## Treatment history
# runTreatmentHistory(con = con,
#                     executionSettings = executionSettings,
#                     analysisSettings = analysisSettings3)
#
# ## Treatment patterns
# executeTreatmentPatterns(con = con,
#                          executionSettings = executionSettings,
#                          analysisSettings = analysisSettings3)

# ## Time to discontinuation
# executeTimeToEvent(con = con,
#                    executionSettings = executionSettings,
#                    analysisSettings = analysisSettings3)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
