# A. Meta Info -----------------------

# Study: Ehden Hmb
# Name: Review Cohort Diagnostics
# Author: Martin Lavallee
# Date: 2023-06-26
# Description: The purpose of 03_reviewCohortDiagnostics.R is to.....

# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(CohortDiagnostics)
library(Ulysses)
library(here)
library(fs)

# C. Script --------------------

configBlock <- "[block]"

# path to cohort diagnostics results
dataFolder <- fs::path(here("results"), configBlock, "02_cohortDiagnostics")


# add a scratch folder
here::here("scratch/data") %>% #from CohortDiagnostics vignette folder must be named data
  fs::dir_create()

#path to sqlite db
sqlLiteDbPath <- here("scratch/data/ehden_hmb.sqlite")

# create merged Results file
createMergedResultsFile(dataFolder = dataFolder,
                        sqliteDbPath = sqlLiteDbPath)


# launch diagnostics
launchDiagnosticsExplorer(sqliteDbPath = sqlLiteDbPath)

# when done reviewing the shiny app hit the stop button in the console.

# F. Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
