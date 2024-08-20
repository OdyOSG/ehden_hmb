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
configBlock <- "[block]"
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
analysisSettings4 <- readSettingsFile(here::here("analysis/settings/treatmentPatternsAnalysis3.yml"))
analysisSettings5 <- readSettingsFile(here::here("analysis/settings/treatmentPatternsAnalysis4.yml"))

# E. Script --------------------

dbType <- config::get("dbms", config = configBlock)

if (dbType == "snowflake") {

  startSnowflakeSession(con = con, executionSettings = executionSettings)

}

## Post index prevalence
executePostIndexDrugUtilization(con = con,
                                executionSettings = executionSettings,
                                analysisSettings = analysisSettings1)


## Treatment History

## Procedures + Drugs (no NSAIDs) for Sequences
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings2)

## Procedures + Drugs (with NSAIDs) for Sequences
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings3)

## Drugs (no NSAIDs) for TTD
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings4)

## Drugs (with NSAIDs) for TTD
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings5)


## Treatment Patterns

## No NSAIDs
executeTreatmentPatterns(con = con,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings2)

## With NSAIDs
executeTreatmentPatterns(con = con,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings3)


## Time to discontinuation

## No NSAIDs
executeTimeToEvent(con = con,
                   executionSettings = executionSettings,
                   analysisSettings = analysisSettings4)

## With NSAIDs
executeTimeToEvent(con = con,
                   executionSettings = executionSettings,
                   analysisSettings = analysisSettings5)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
