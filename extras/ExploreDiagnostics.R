# File to review cohort diagnostic results

# A. File info -------------

# The purpose of this file is to review Cohort Diagnostics results


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(CohortDiagnostics)
library(Ulysses)
library(fs)


# C. Script --------------------

databaseName <- "[database]" # the databaseName to use for results sharing

## Path to cohort diagnostics results
dataFolder <- fs::path_abs("results") %>%
  fs::path(databaseName, "02_cohortDiagnostics")


## Add a scratch folder
scratchDiagnosticsFolder <- fs::path_abs("scratchDiagnostics") %>%   # From CohortDiagnostics vignette: Folder must be named "data"
  fs::dir_create()
usethis::use_git_ignore("scratchDiagnostics")

## Path to sqlite db
sqlLiteDbPath <- fs::path(scratchDiagnosticsFolder, glue::glue("ehden_cd_{databaseName}"), ext = "sqlite")

# create merged Results file
CohortDiagnostics::createMergedResultsFile(dataFolder = dataFolder,
                                           sqliteDbPath = sqlLiteDbPath)


## Launch diagnostics
CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlLiteDbPath)

## NOTE: When done reviewing the shiny app hit the stop button in the console

