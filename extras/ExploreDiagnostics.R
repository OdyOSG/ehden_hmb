# File to review cohort diagnostic results

# A) File info -------------

# The purpose of this file is to review cohort diagnostics once they have been created
# Author: Martin Lavallee
# Date: 09/19/2023

# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(CohortDiagnostics)
library(Ulysses)
library(fs)

# C. Script --------------------

databaseName <- "[databaseName]" #the databaseName to use for results sharing

# path to cohort diagnostics results
dataFolder <- fs::path_abs("results") %>%
  fs::path(databaseName, "02_cohortDiagnostics")


# add a scratch folder
scratchDiagnosticsFolder <- fs::path_abs("scratchDiagnostics") %>% #from CohortDiagnostics vignette folder must be named data
  fs::dir_create()
usethis::use_git_ignore("scratchDiagnostics")

#path to sqlite db
sqlLiteDbPath <- fs::path(scratchDiagnosticsFolder, glue::glue("ehden_cd_{databaseName}"), ext = "sqlite")

# create merged Results file
CohortDiagnostics::createMergedResultsFile(dataFolder = dataFolder,
                        sqliteDbPath = sqlLiteDbPath)


# launch diagnostics
CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlLiteDbPath)

# when done reviewing the shiny app hit the stop button in the console.

