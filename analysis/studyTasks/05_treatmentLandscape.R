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

## set options Ex. options(connectionObserver = NULL)

## set source files source('my_file.R')


# C. Connection ----------------------

# set connection Block
configBlock <- "[Add config block]"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = config::get("dbms",
                                                                                   config = configBlock),
  user = config::get("user",
                     config = configBlock), password = config::get("password",
                                                                   config = configBlock),
  connectionString = config::get("connectionString", config = configBlock))

# connect to database
con <- DatabaseConnector::connect(connectionDetails)
withr::defer(expr = DatabaseConnector::disconnect(con), envir = parent.frame())  #close on exit


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "05_treatmentLandscape") %>%
  fs::dir_create()

### Add study variables or load from settings

# E. Script --------------------

# Add script here

# F. Session Info ------------------------

sessioninfo::session_info()
rm(list = ls())
withr::deferred_run()
