# 05_treatmentLandscape.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Treatment Landscape
# Author: Martin Lavallee
# Date: 08/29/2023
# Description: The purpose of this script is to run the treatment patterns module

# B. Dependencies ----------------------

## include R libraries
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

# connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

### Analysis Settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/treatmentPatterns.yml"))


# E. Script --------------------

#######if BAYER uncomment this line#################
#startSnowflakeSession(con, executionSettings)

# execute post index prevalence
executePostIndexDrugUtilization(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettings)

# run treatment history
runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings)

# get treatment patterns
executeTreatmentPatterns(con = con,
                     executionSettings = executionSettings,
                     analysisSettings = analysisSettings)

#get time to discontinuation
executeTimeToEvent(con = con,
               executionSettings = executionSettings,
               analysisSettings = analysisSettings)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
