# 04_baselineCharacteristics.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Baseline Characteristics
# Author: Martin Lavallee
# Date: 07/20/2023
# Description: The purpose of this script is to run baseline characteristics for the ehden study

# B. Dependencies ----------------------

## include R libraries
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_utilities.R")
source("analysis/private/_conceptPrevalence.R")
source("analysis/private/_cohortPrevalence.R")
source("analysis/private/_conditionRollup.R")
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

# connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

### Analysis Settings
analysisSettings1 <- readSettingsFile(here::here("analysis/settings/baselineCharacteristics.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/underlyingConditions.yml"))
# E. Script --------------------

#######if BAYER uncomment this line#################
startSnowflakeSession(con, executionSettings)

## Get Baseline Covariates

# run concept characterization
executeConceptCharacterization(con = con,
                               executionSettings = executionSettings,
                               analysisSettings = analysisSettings1)

#run cohort characterization
executeCohortPrevalence(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettings1)

#run icd chapters rollup
executeConditionRollup(con = con,
                       executionSettings = executionSettings,
                       analysisSettings = analysisSettings1)

#run cohort prevalence for underlying
executeCohortPrevalence(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettings2)

# F. Session Info ------------------------
DatabaseConnector::disconnect(con)
