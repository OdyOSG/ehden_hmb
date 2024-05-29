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
#databaseNameInci <- c(databaseName, "mrktscan2")
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
  dplyr::select(-cohortDefinitionId, -timeWindow) %>%
  maskLowCount()

## Cohort baseline
cohortChar <- readr::read_csv(fs::path(dataPath, "baselineCohorts.csv"),
                              show_col_types = FALSE) %>%
  dplyr::rename(covariateName2 = covariateName) %>%
  dplyr::mutate(
    domain = dplyr::case_when(
      covariateName2 %in% c("antidepressants", "antipsychotics", "antithrombotics", "nsaids", "grha", "tranexamicAcid",
                           "tamoxifen", "gonadalSteroids", "copperIUDdrug", "danazol", "lngIUD", "ulipristalAcetate",
                           "oc_estradiolDienogest", "oc_other", "progestinOnly", "ironPreparations",
                           "oralContraceptives_estradiolDienogest", "oralContraceptives_other") ~ "Drugs",
      covariateName2 %in% c("copperIUDprocedure", "bloodTransfusion", "hysterectomy", "myomectomy",
                           "uae", "undefinedIUD", "endometrialAblation", "hormonalIUD") ~ "Procedures",
      TRUE ~ "Conditions"
    )
  ) %>%
  dplyr::select(databaseId, cohortName, domain, covariateName2, count, pct) %>%
  maskLowCount()

cohortChar$covariateName <- gsub("oralContraceptives_estradiolDienogest", "oc_estradiolDienogest", cohortChar$covariateName2)
cohortChar$covariateName <- gsub("oralContraceptives_other", "oc_other", cohortChar$covariateName)
cohortChar$covariateName2 <- NULL
cohortChar <- cohortChar %>% dplyr::select(databaseId, cohortName, domain, covariateName, count, pct)

## Chapters baseline
icdChar <- readr::read_csv(fs::path(dataPath, "baselineChapters.csv"),
                           show_col_types = FALSE) %>%
  dplyr::select(-COHORT_ID, -timeWindow) %>%
  dplyr::rename(count = COUNTVALUE) %>%
  maskLowCount()


### Baseline Pickers
domainConceptChar <- sort(unique(conceptChar$domain))
domainCohortChar  <- sort(unique(cohortChar$domain))

# # 3. Incidence -----------------
#
# ## Incidence
# incTab <- readr::read_csv(fs::path(dataPath, "incidence.csv"),
#                           show_col_types = FALSE) %>%
#   dplyr::select(-c(OUTCOME_COHORT_DEFINITION_ID, AGE_ID)) %>%
#   dplyr::mutate(PERSON_YEARS = as.integer(PERSON_DAYS/365.25), .before =6) %>%
#   maskLowCountInci()
#
# ### Incidence Pickers
# yearInci <- c("All", as.character(2000:2022))
# ageInci <- unique(incTab$AGE_GROUP_NAME) %>% sort(decreasing = TRUE)
# cohortNameInciPlot <- "hmb"
# ageGroupInciPlot <- unique(incTab$AGE_GROUP_NAME) %>% sort(decreasing = TRUE)
# databaseInci <- unique(incTab$databaseId)

# 4. PostIndex Prevalence -----------------

postIndex <- readr::read_csv(fs::path(dataPath, "postIndexPrevalence.csv"),
                             show_col_types = FALSE) %>%
  dplyr::select(-cohortId, -covariateId) %>%
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
  dplyr::select(databaseId, timeWindow, cohortName, covariateName, count, pct, cat) %>%
  dplyr::arrange(databaseId, timeWindow) %>%
  dplyr::mutate(timeWindow = as.character(timeWindow),
                cat = dplyr::case_when(
                  cat == "within" ~ "Within time window",
                  cat == "followUp" ~ "Complete follow-up"
                  )
                )

### Pickers
drugCohorts <- unique(drugPi$covariateName)
drugTimeWindow <- unique(drugPi$timeWindow)
drugCategory <- unique(drugPi$cat)

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
txPatDat <- readr::read_csv(fs::path(dataPath, "treatmentPatternsAll.csv"),
                            show_col_types = FALSE)

txPatDat6m <- readr::read_csv(fs::path(dataPath, "treatmentPatterns6m.csv"),
                             show_col_types = FALSE)

txPatDat1y <- readr::read_csv(fs::path(dataPath, "treatmentPatterns1y.csv"),
                              show_col_types = FALSE)

txPatDat2y <- readr::read_csv(fs::path(dataPath, "treatmentPatterns2y.csv"),
                              show_col_types = FALSE)

txPatDatNsaids <- readr::read_csv(fs::path(dataPath, "treatmentPatternsAllNsaids.csv"),
                            show_col_types = FALSE)

txPatDat6mNsaids <- readr::read_csv(fs::path(dataPath, "treatmentPatterns6mNsaids.csv"),
                              show_col_types = FALSE)

txPatDat1yNsaids <- readr::read_csv(fs::path(dataPath, "treatmentPatterns1yNsaids.csv"),
                              show_col_types = FALSE)

txPatDat2yNsaids <- readr::read_csv(fs::path(dataPath, "treatmentPatterns2yNsaids.csv"),
                              show_col_types = FALSE)

txPatDatAll <- dplyr::bind_rows(
  txPatDat,
  txPatDat6m,
  txPatDat1y,
  txPatDat2y,
  txPatDatNsaids,
  txPatDat6mNsaids,
  txPatDat1yNsaids,
  txPatDat2yNsaids
)

### Sankey pickers
cohortName2 <- c("hmb", "hmb_age_lt_30", "hmb_age_30_45", "hmb_age_45_55")

sankeyCohorts <- tibble::tibble(
  id = c(1L, 1001L, 1002L, 1003L),
  name = cohortName2
)

txTime <- unique(txPatDatAll$time)
txType <- unique(txPatDatAll$type)


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

ttd2 <- arrow::read_parquet(file = fs::path(here::here(dataPath ,"ttd2.parquet"))) %>%
  dplyr::mutate(targetId = as.double(targetId)) %>%
  dplyr::left_join(strataCounts, by = c("targetId" = "Strata Cohort Id", "database" = "Database")) %>%
  dplyr::rename(`Cohort Name` = `Strata Cohort Name`) %>%
  dplyr::select(database:`Cohort Name`) %>%
  dplyr::mutate(`Cohort Name` = dplyr::case_when(
    targetId == 1 ~ "hmb",
    TRUE ~ `Cohort Name`)
  )

### Relabel strata
# ttd <- relabelStrata(
#   ttd,
#   oldLabels = as.character(c(27:29, 31:35)),
#   newLabels = c("oc", "danazol", "grha", "lglIUD",
#                 "nsaids", "progestin", "tranexamicAcid", "ulipristalAcetate")
# )

### TTE cohort pickers
ttdCohorts <- unique(ttd$`Cohort Name`)

### TTE pickers
tteCohorts <- tibble::tibble(
  id = c(1, 1001L, 1002L, 1003L),
  name = cohortName
)

## TTD Line pickers
ttdLine <- unique(ttd$line)


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
  oldLabels = as.character(38:45),
  newLabels = c("bloodTransfusion", "copperIUD", "endometrialAblation", "hormonalIUD",
                "hysterectomy", "myomectomy", "uae", "undefinedIUD")
)

### TTI cohort pickers
ttiCohorts <- unique(tti$`Cohort Name`)

