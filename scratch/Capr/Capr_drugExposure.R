# Capr_drugExposure.R

# A. File Info -----------------------

# Study: Ehden Hmb
# Name: Capr Script for drugExposure
# Author: Martin Lavallee
# Date: 07/20/2023
# Description: The purpose of this Capr script is to develop drug exposure cohorts
# for the HMB study

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


cohortFolder <- "drugExposure" %>% #if this is the target cohort do not make new folder
  Ulysses::addCohortFolder()
cohortFolder <- fs::path(here::here("cohortsToCreate"), "03_drugExposure")

# E. Concept Sets --------------------

## Drug Concepts ----------

ehden_drugs <- list(

  'tranexamicAcid' = cs(
    descendants(
      21601036 #tranexamic acid
    ),
    name = "tranexamic Acid"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'progestin' = cs(
    descendants(
      21602502, # medroxyprogesterone; parenteral
      21602553, #norethisterone; oral
      21602505, # desogestrel; systemic
      21602504 # eonogestrel; implant
    ),
    name = "Progestin"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'nsaids' = cs(
    descendants(
      21603933 #nsaids
    ),
    name = "nsaids"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'combOralContraceptives' = cs(
    descendants(
      40253998, #dienogest and estradiol
      21602487 #nomegestrol and estradiol
    ),
    name = "combined Oral Hormonal Contraceptives"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'progresteron' = cs(
    descendants(
      40254011, # ulipristal; oral (emergency contraceptives)
      43534787 # ulipristal; oral (progesterone receptor modulators)
    ),
    name = "Selective progesteron receptor modulators"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'danazol' = cs(
    descendants(
      21602620 #danazol; oral
    ),
    name = "danazol"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'grha' = cs(
    descendants(
      21603823 #	Gonadotropin releasing hormone analogues
    ),
    name = "Gonadotropin releasing hormone analogues"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'iud' = cs(
    descendants(
      21602446 #	Intrauterine contraceptives
    ),
    name = "IUD"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema),

  'ironPreparations' = cs(
    descendants(
      21601078 #IRON PREPARATIONS
    ),
    name = "Iron Preparations"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema)
)



# F. Cohort Definition ----------------

ehdenDrugCohortTemplate <- function(conceptSet, name, cohortFolder) {

  # Concept sets for censoring
  oophorectomy <- cs(
    descendants(4297990), name = "bilateral oophorectomy"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema)


  hysterectomy <- cs(
    descendants(4127886), name = "hysterectomy"
  ) %>%
    getConceptSetDetails(con = con, vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema)


  menopause <- cs(
    descendants(
      193739,201078,4010333,4059477,4087072,4141640,4172857,42536667
    ), name = "menopause"
  )

  # build Cohort definition
  cd <- cohort(
    entry = entry(
      drugExposure(conceptSet = conceptSet),
      observationWindow = continuousObservation(priorDays = 365, postDays = 0),
      primaryCriteriaLimit = "All"
    ),
    exit = exit(
      endStrategy = drugExit(conceptSet = conceptSet,
                             persistenceWindow = 30L),
      censor = censoringEvents(
        procedure(oophorectomy),
        procedure(hysterectomy),
        conditionOccurrence(menopause),
        observation(menopause)
      )
    ),
    era = era(eraDays = 30L)
  )
  txt <- glue::glue("Writing cohort definition {crayon::green(name)} to {crayon::cyan(cohortFolder)}")
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")
  writeCohort(cd, path = fs::path(cohortFolder, name, ext = "json"))
  invisible(cd)
}

purrr::walk2(ehden_drugs, names(ehden_drugs),
             ~ehdenDrugCohortTemplate(
               conceptSet = .x,
               name = .y,
               cohortFolder = cohortFolder)
)

# fix visit, death and qualified limit by hand

# F. Session Info ------------------------

sessioninfo::session_info()
withr::deferred_run()
rm(list = ls())
