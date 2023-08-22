# 05_treatmentLandscape.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Treatment Landscape
# Author: Martin Lavallee
# Date: [Add Date]
# Description: The purpose of this script is to.....

# B. Dependencies ----------------------

## include R libraries
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)

source("analysis/private/_treatmentHistory_helpers.R")
source("analysis/private/_treatmentHistory.R")
source("analysis/private/_treatmentPatterns.R")
source("analysis/private/_utilities.R")


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
analysisSettings <- readSettingsFile(here::here("analysis/settings/treatmentLandscape.yml"))


# E. Script --------------------

#######if BAYER uncomment this line#################
startSnowflakeSession(con, executionSettings)


executeCohortPrevalence(con = con,
                        executionSettings = executionSettings,
                        analysisSettings = analysisSettings)

runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings)


getTreatmentPatterns(con = con,
                     executionSettings = executionSettings,
                     analysisSettings = analysisSettings)

# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
