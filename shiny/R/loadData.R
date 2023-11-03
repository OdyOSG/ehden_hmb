# Load data into environment for app


# Dependencies -------------------

library(dplyr)
library(readr)

source(here::here("shiny", "R", "fn.R"))
source(here::here("extras/helpers.R"))

dataPath <- here::here("shiny", "data")


# About  ------------------------
## Load database meta
# databaseMeta <- readr::read_csv(fs::path(dataPath, "databaseMeta.csv"),
#                                 show_col_types = FALSE)


# Cohorts  ------------------------
## Load cohort Counts
cohortCounts <- readr::read_csv(fs::path(dataPath, "cohortCounts.csv"),
                                show_col_types = FALSE)

## Load strata counts
strataCounts <- readr::read_csv(fs::path(dataPath, "strataCounts.csv"),
                                show_col_types = FALSE)

## Global Pickers ---------------------------
databaseName <- unique(cohortCounts$Database)
cohortName <- c("hmb", "hmb age_lt_30", "hmb age_30_45", "hmb age_45_55")



# Clinical Characteristics  ------------------------
## Load demographics baseline

demoChar <- readr::read_csv(fs::path(dataPath, "baselineDemographics.csv"),
                            show_col_types = FALSE) %>%
  dplyr::rename(count = n) %>%
  maskLowCount()

## Load continuous baseline
ctsChar <- readr::read_csv(fs::path(dataPath, "baselineContinuous.csv"),
                           show_col_types = FALSE)

## Load concept baseline
conceptChar <- readr::read_csv(fs::path(dataPath, "baselineConcepts.csv"),
                               show_col_types = FALSE) %>%
  dplyr::rename(count = n) %>%
  maskLowCount()

## Load cohort baseline
cohortChar <- readr::read_csv(fs::path(dataPath, "baselineCohorts.csv"),
                               show_col_types = FALSE) %>%
  dplyr::mutate(
    domain = dplyr::case_when(
      covariateName %in% c("antidepressants", "antipsychotics", "antithrombotics",
        "tamoxifen", "gonadalSteroids", "copperIUDdrug") ~ "Drugs",
      TRUE ~ "Conditions"
    )
  ) %>%
  dplyr::select(
    databaseId, timeWindow, cohortId, cohortName,
    domain, covariateId, covariateName, count, pct
  ) %>%
  maskLowCount()

## Load chapters baseline
icdChar <- readr::read_csv(fs::path(dataPath, "baselineChapters.csv"),
                              show_col_types = FALSE) %>%
  dplyr::rename(count = COUNTVALUE) %>%
  maskLowCount()


## Baseline Pickers
domainConceptChar   <- sort(unique(conceptChar$domain))


# Incidence -----------------------

## Incidence
incTab <- readr::read_csv(fs::path(dataPath, "incidence.csv"),
                           show_col_types = FALSE)

### Incidence Pickers
yearInci <- c("All", as.character(2000:2022))


# PostIndex Prevalence  ------------------------------------

postIndex <- readr::read_csv(fs::path(dataPath, "postIndexPrevalence.csv"),
                             show_col_types = FALSE) %>%
  maskLowCount()

## Underlying conditions--------------------

condPi <- postIndex %>%
  dplyr::filter(type == "conditions") %>%
  dplyr::select(databaseId, timeWindow, cohortId, cohortName, covariateId, covariateName,
                count, pct) %>%
  dplyr::arrange(databaseId, cohortId, timeWindow, covariateId)

### Pickers
condCohorts <- unique(condPi$covariateName)
condTimeWindow <- unique(condPi$timeWindow)

## Drug Utilization------------------

drugPi <- postIndex %>%
  dplyr::filter(type == "drugs") %>%
  dplyr::mutate(
    timeWindow = factor(timeWindow, levels = c("1d - 183d", "184d - 365d", "1d - 365d", "366d - 730d", "731d - 1825d"))
  ) %>%
  dplyr::select(databaseId, timeWindow, cohortId, cohortName, covariateId, covariateName,
                count, pct) %>%
  dplyr::arrange(databaseId, cohortId, timeWindow, covariateId) %>%
  dplyr::mutate(
    timeWindow = as.character(timeWindow)
  )

### Pickers
drugCohorts <- unique(drugPi$covariateName)
drugTimeWindow <- unique(drugPi$timeWindow)

## Procedures--------------------

procPi <- postIndex %>%
  dplyr::filter(type == "procedures") %>%
  dplyr::select(databaseId, timeWindow, cohortId, cohortName, covariateId, covariateName,
                count, pct)


### Pickers
procCohorts <- unique(procPi$covariateName)
procTimeWindow <- unique(procPi$timeWindow)


# Treatment Patterns ----------------------

## Load treatment patterns table
txPatDat <- readr::read_csv(fs::path(dataPath, "treatmentPatterns.csv"),
                            show_col_types = FALSE)


txPatDat2 <- readr::read_csv(fs::path(dataPath, "treatmentPatterns2.csv"),
                            show_col_types = FALSE)

txPatDatAll <- dplyr::bind_rows(
  txPatDat,
  txPatDat2
)

## sankey pickers
cohortName2 <- c(
  cohortName,
  "hmb2", "hmb2 age_lt_30", "hmb2 age_30_45", "hmb2 age_45_55"
  )

sankeyCohorts <- tibble::tibble(
  id = c(1L, 1001L, 1002L, 1003L, 44L, 44001L, 44002L, 44003L),
  name = cohortName2
)


# Time to Event -----------------------

## Time to Discontinuation -----------------

ttd <- arrow::read_parquet(file = fs::path(here::here(dataPath ,"ttd.parquet")))

# relabel strata
ttd <- relabelStrata(
  ttd,
  oldLabels = as.character(c(27:29, 31:35)),
  newLabels = c("oc", "danazol", "grha", "lglIUD",
                "nsaids", "progestin", "tranexamicAcid", "ulipristalAcetate")
)

## TTE pickers
tteCohorts <- tibble::tibble(
  id = c(1, 1001L, 1002L, 1003L),
  name = cohortName
)

## Time to Intervention -----------------

tti <- arrow::read_parquet(file = fs::path(here::here(dataPath ,"tti.parquet")))


# relabel strata
tti <- relabelOutcome(
  tti,
  oldLabels = as.character(36:43),
  newLabels = c("bloodTransfusion", "copperIUD", "endoemtrialAblation", "hormonalIUD",
                "hysterectomy", "myomectomy", "uae", "undefinedIUD")
)
