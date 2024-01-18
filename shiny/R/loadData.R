# Load data into environment for app

# Dependencies -----------------

library(dplyr)
library(readr)

source(here::here("shiny", "R", "fn.R"))
source(here::here("shiny", "migration", "helpers.R"))

dataPath <- here::here("shiny", "data")


# About -----------------
## Load database meta
# databaseMeta <- readr::read_csv(fs::path(dataPath, "databaseMeta.csv"),
#                                 show_col_types = FALSE)


# 1. Cohorts -----------------
## Load cohort Counts
cohortCounts <- readr::read_csv(fs::path(dataPath, "cohortCounts.csv"),
                                show_col_types = FALSE)

## Load strata counts
strataCounts <- readr::read_csv(fs::path(dataPath, "strataCounts.csv"),
                                show_col_types = FALSE)

### Global Pickers ---------------------------
databaseName <- unique(cohortCounts$Database)
cohortName <- c("hmb", "hmb age_lt_30", "hmb age_30_45", "hmb age_45_55")



# 2. Clinical Characteristics -----------------

## Demographics baseline
demoChar <- readr::read_csv(fs::path(dataPath, "baselineDemographics.csv"),
                            show_col_types = FALSE) %>%
  dplyr::rename(count = n) %>%
  dplyr::select(-cohortDefinitionId) %>%
  maskLowCount()

## Continuous baseline
ctsChar <- readr::read_csv(fs::path(dataPath, "baselineContinuous.csv"),
                           show_col_types = FALSE) %>%
  dplyr::select(-cohortDefinitionId)

## Concept baseline
conceptChar <- readr::read_csv(fs::path(dataPath, "baselineConcepts.csv"),
                               show_col_types = FALSE) %>%
  dplyr::rename(count = n) %>%
  dplyr::select(-cohortDefinitionId) %>%
  maskLowCount()

## Cohort baseline
cohortChar <- readr::read_csv(fs::path(dataPath, "baselineCohorts.csv"),
                              show_col_types = FALSE) %>%
  dplyr::mutate(
    domain = dplyr::case_when(
      covariateName %in% c("antidepressants", "antipsychotics", "antithrombotics",
                           "tamoxifen", "gonadalSteroids", "copperIUDdrug") ~ "Drugs",
      TRUE ~ "Conditions"
    )
  ) %>%
  dplyr::select(databaseId, timeWindow, cohortName, domain, covariateName, count, pct) %>%
  dplyr::mutate(timeWindow = dplyr::case_when(
    timeWindow == "[-365 : 0]" ~ "T(-365d to 0d)",
    TRUE ~ timeWindow
  )) %>%
  maskLowCount()

## Chapters baseline
icdChar <- readr::read_csv(fs::path(dataPath, "baselineChapters.csv"),
                           show_col_types = FALSE) %>%
  dplyr::select(-COHORT_ID) %>%
  dplyr::rename(count = COUNTVALUE) %>%
  maskLowCount()


### Baseline Pickers
domainConceptChar <- sort(unique(conceptChar$domain))


# 3. Incidence -----------------

## Incidence
incTab <- readr::read_csv(fs::path(dataPath, "incidence.csv"),
                          show_col_types = FALSE) %>%
  dplyr::select(-OUTCOME_COHORT_DEFINITION_ID)  %>%
  maskLowCountInci()

### Incidence Pickers
yearInci <- c("All", as.character(2000:2022))


# 4. PostIndex Prevalence -----------------

postIndex <- readr::read_csv(fs::path(dataPath, "postIndexPrevalence.csv"),
                             show_col_types = FALSE) %>%
  dplyr::select(-cohortId, -covariateId) %>%
  dplyr::mutate(timeWindow = dplyr::case_when(
    timeWindow == "1d - 365d" ~ "T(1d to 365d)",
    timeWindow == "1d - 183d" ~ "T(1d to 183d)",
    timeWindow == "184d - 365d" ~ "T(184d to 365d)",
    timeWindow == "366d - 730d" ~ "T(366d to 730d)",
    timeWindow == "731d - 1825d" ~ "T(731d to 1825d)",
    timeWindow == "1d - 9999d" ~ "T(1d to 9999d)",
    TRUE ~ timeWindow
  )) %>%
  maskLowCount()


## Underlying conditions -----------------

condPi <- postIndex %>%
  dplyr::filter(type == "conditions") %>%
  dplyr::select(databaseId, timeWindow, cohortName, covariateName, count, pct) %>%
  dplyr::arrange(databaseId, timeWindow)

### Pickers
condCohorts <- unique(condPi$covariateName)
condTimeWindow <- unique(condPi$timeWindow)


## Drug Utilization -----------------

drugPi <- postIndex %>%
  dplyr::filter(type == "drugs") %>%
  # dplyr::mutate(
  #   timeWindow = factor(timeWindow, levels = c("1d - 183d", "184d - 365d", "1d - 365d", "366d - 730d", "731d - 1825d"))
  # ) %>%
  dplyr::select(databaseId, timeWindow, cohortName, covariateName, count, pct) %>%
  dplyr::arrange(databaseId, timeWindow) %>%
  dplyr::mutate(timeWindow = as.character(timeWindow))

### Pickers
drugCohorts <- unique(drugPi$covariateName)
drugTimeWindow <- unique(drugPi$timeWindow)


## Procedures -----------------

procPi <- postIndex %>%
  dplyr::filter(type == "procedures") %>%
  dplyr::select(databaseId, timeWindow, cohortName, covariateName, count, pct)

### Pickers
procTarget <- unique(procPi$cohortName)
procCohorts <- unique(procPi$covariateName)
procTimeWindow <- unique(procPi$timeWindow)


# 5. Treatment Patterns -----------------

## Load treatment patterns table
txPatDat <- readr::read_csv(fs::path(dataPath, "treatmentPatterns.csv"),
                            show_col_types = FALSE)


txPatDat2 <- readr::read_csv(fs::path(dataPath, "treatmentPatterns2.csv"),
                             show_col_types = FALSE)

txPatDatAll <- dplyr::bind_rows(
  txPatDat,
  txPatDat2
)

### Sankey pickers
cohortName2 <- c(
  cohortName,
  "hmb2", "hmb2_age_lt_30", "hmb2_age_30_45", "hmb2_age_45_55"
)

sankeyCohorts <- tibble::tibble(
  id = c(1L, 1001L, 1002L, 1003L, 44L, 44001L, 44002L, 44003L),
  name = cohortName2
)


# 6. Time to Event -----------------

## Time to Discontinuation -----------------

ttd <- arrow::read_parquet(file = fs::path(here::here(dataPath ,"ttd.parquet"))) %>%
  dplyr::mutate(targetId = as.double(targetId)) %>%
  dplyr::left_join(strataCounts, by = c("targetId" = "Strata Cohort Id", "database" = "Database")) %>%
  dplyr::rename(`Cohort Name` = `Strata Cohort Name`) %>%
  dplyr::select(database:`Cohort Name`) %>%
  dplyr::mutate(`Cohort Name` = dplyr::case_when(
    targetId == 1 ~ "hmb",
    TRUE ~ `Cohort Name`)
  )

### Relabel strata
ttd <- relabelStrata(
  ttd,
  oldLabels = as.character(c(27:29, 31:35)),
  newLabels = c("oc", "danazol", "grha", "lglIUD",
                "nsaids", "progestin", "tranexamicAcid", "ulipristalAcetate")
)

### TTE cohort pickers
ttdCohorts <- unique(ttd$`Cohort Name`)

### TTE pickers
tteCohorts <- tibble::tibble(
  id = c(1, 1001L, 1002L, 1003L),
  name = cohortName
)


## Time to Intervention -----------------

tti <- arrow::read_parquet(file = fs::path(here::here(dataPath ,"tti.parquet"))) %>%
  dplyr::mutate(targetId = as.double(targetId)) %>%
  dplyr::left_join(strataCounts, by = c("targetId" = "Strata Cohort Id", "database" = "Database")) %>%
  dplyr::rename(`Cohort Name` = `Strata Cohort Name`) %>%
  dplyr::select(database:`Cohort Name`) %>%
  dplyr::mutate(`Cohort Name` = dplyr::case_when(
    targetId == 1 ~ "hmb",
    TRUE ~ `Cohort Name`)
    )



### Relabel strata
tti <- relabelOutcome(
  tti,
  oldLabels = as.character(36:43),
  newLabels = c("bloodTransfusion", "copperIUD", "endometrialAblation", "hormonalIUD",
                "hysterectomy", "myomectomy", "uae", "undefinedIUD")
)

### TTI cohort pickers
ttiCohorts <- unique(tti$`Cohort Name`)
