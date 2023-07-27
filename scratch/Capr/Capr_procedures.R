# Capr_procedures.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Capr Script for procedures
# Author: Martin Lavallee
# Date: [Add Date]
# Description: The purpose of this Capr script is to develop xxx cohorts....

# B. Dependencies ----------------------

## include R libraries
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(Capr)

# C. Connection ----------------------

# set connection Block
configBlock <- "[add block]"

# provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms",config = configBlock),
  user = config::get("user",config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

# connect to database
con <- DatabaseConnector::connect(connectionDetails)
withr::defer(expr = DatabaseConnector::disconnect(con), envir = parent.frame())  #close on exit


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))


cohortFolder <- "procedures" %>% #if this is the target cohort do not make new folder
  Ulysses::addCohortFolder()
cohortFolder <- fs::path(here::here("cohortsToCreate"), "05_procedures")

# E. Concept Sets --------------------

ehden_procedures <- list(
  'hysterectomy' = cs(
    descendants(4127886), name = "hysterectomy"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'endometrialAblation' = cs(
    descendants(4141940),  name = "endometrial Ablation"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'uae' = cs(
    descendants(4193984), name = "Uterine artery embolization"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'myomectomy' = cs(
    descendants(4169931),name = "Uterine myomectomy"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema)
)



# F. Cohort Definition ----------------

ehdenProcedureTemplate <- function(conceptSet, name, cohortFolder) {

  # build Cohort definition
  cd <- cohort(
    entry = entry(
      procedure(conceptSet = conceptSet),
      observationWindow = continuousObservation(priorDays = 365, postDays = 0),
      primaryCriteriaLimit = "First"
    ),
    exit = exit(
      endStrategy = fixedExit(index = "startDate", offsetDays = 0L)
      )
    )

  txt <- glue::glue("Writing cohort definition {crayon::green(name)} to {crayon::cyan(cohortFolder)}")
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")
  writeCohort(cd, path = fs::path(cohortFolder, name, ext = "json"))
  invisible(cd)
}

purrr::walk2(ehden_procedures, names(ehden_procedures),
             ~ehdenProcedureTemplate(
               conceptSet = .x,
               name = .y,
               cohortFolder = cohortFolder)
)

# fix visit, death and qualified limit by hand
# F. Session Info ------------------------

sessioninfo::session_info()
rm(list = ls())
withr::deferred_run()
