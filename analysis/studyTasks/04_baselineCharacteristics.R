# A. File Info -----------------------

# Name: Baseline Characteristics
# Description: The purpose of this script is to run baseline characteristics for the ehden study


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_utilities.R")
source("analysis/private/_conceptPrevalence.R")
source("analysis/private/_cohortPrevalence.R")
source("analysis/private/_conditionRollup.R")


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
analysisSettings1 <- readSettingsFile(here::here("analysis/settings/baselineCharacteristics.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/underlyingConditions.yml"))


# E. Script --------------------

startSnowflakeSession(con =con, executionSettings = executionSettings)

## Concept characterization
executeConceptCharacterization(con = con,
                               executionSettings = executionSettings,
                               analysisSettings = analysisSettings1)

## Cohort characterization
executeCohortPrevalence(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettings1)

## ICD10 chapters rollup
executeConditionRollup(con = con,
                       executionSettings = executionSettings,
                       analysisSettings = analysisSettings1)

## Cohort prevalence for underlying conditions
executeCohortPrevalence(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettings2)


# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
