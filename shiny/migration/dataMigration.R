# Dependencies ----------------

library(tidyverse)
library(dplyr)
library(readr)

source("shiny/migration/helpers.R")


appDataPath <- here::here("shiny", "data_old") # Path to place app data

appDataPath %>% fs::dir_create() # Create new directory

resultsPath <- here::here("results") # Set path to the execution results

listOfDatabase <- fs::dir_ls(resultsPath) %>% #list the databases used in the execution
  basename()

### List the execution tasks
listOfTasks <- c("01_buildCohorts",
                 "02_cohortDiagnostics",
                 "03_buildStrata",
                 "04_incidenceAnalysis",
                 "05_baselineCharacteristics",
                 "06_postIndexPrevalenceConditions",
                 "07_postIndexPrevalenceDrugs",
                 "08_treatmentHistory",
                 "09_treatmentPatterns",
                 "10_timeToDiscontinuation",
                 "11_postIndexPrevalenceProcedures",
                 "12_timeToIntervention",
                 "13_treatmentHistory2",
                 "14_treatmentPatterns")

### Create a data frame of all permutations of paths
allPaths <- tidyr::expand_grid(listOfDatabase, listOfTasks) %>%
  dplyr::mutate(
    fullPath = fs::path(resultsPath, listOfDatabase, listOfTasks)
  )


## 1. Bind and save Cohort Manifest for all databases ----------------

cohortManifest <- bindCsv(allPaths = allPaths,
                          task = listOfTasks[1],       # Cohorts
                          file = "cohortManifest.csv")
cm2 <- cohortManifest %>%
  dplyr::select(databaseId, id, name, entries, subjects) %>%
  dplyr::rename(
    Database = databaseId,
    `Cohort Id` = id,
    `Cohort Name` = name,
    Entries = entries,
    Subjects = subjects
  ) %>%
  dplyr::mutate(Entries = dplyr::if_else(is.na(Entries), 0, Entries, 0),
                Subjects = dplyr::if_else(is.na(Subjects), 0, Subjects, 0)
  ) %>%
  dplyr::mutate(Entries = dplyr::if_else(Entries <= 5L & Entries > 0, "<5", format(Entries, big.mark = ",", scientific = FALSE), "0"),
                Subjects = dplyr::if_else(Subjects <= 5L & Subjects > 0, "<5", format(Subjects, big.mark = ",", scientific = FALSE), "0")
  )

readr::write_csv(cm2, file = fs::path(appDataPath, "cohortCounts.csv"))


## 2. Bind and save strata table for all databases ----------------

strataManifest <- bindCsv(allPaths = allPaths,
                          task = listOfTasks[3],     # Strata
                          file = "strata_table.csv")

sm2 <- strataManifest %>%
  dplyr::rename(
    Database = databaseId,
    `Strata Cohort Id` = cohort_definition_id,
    `Strata Cohort Name` = name,
    Subjects = n
  )%>%
  dplyr::mutate(Subjects = dplyr::if_else(is.na(Subjects), 0, Subjects, 0)
  ) %>%
  dplyr::mutate(Subjects = dplyr::if_else(Subjects <= 5L & Subjects > 0, "<5", format(Subjects, big.mark = ",", scientific = FALSE), "0"))


readr::write_csv(sm2, file = fs::path(appDataPath, "strataCounts.csv"))

### Bind all cohorts together for full list
allCohorts <- dplyr::bind_rows(
  cohortManifest %>%
    dplyr::rename(cohortName = name) %>%
    dplyr::select(id, cohortName) %>%
    dplyr::distinct(),
  strataManifest %>%
    dplyr::rename(id = cohort_definition_id,
                  cohortName = name) %>%
    dplyr::select(id, cohortName) %>%
    dplyr::distinct()
)


## 3. Baseline Demographics ----------------
`%notin%` <- Negate("%in%")

demo <- bindCsv(allPaths = allPaths,
                task = listOfTasks[5], # baseline char
                file = "demographics_baseline.csv")

demo2 <- demo %>%
  dplyr::mutate(
    Covariate = dplyr::case_when(
      # gender
      analysisId == 1 & conceptId == 8507 ~ "Gender: Male",
      analysisId == 1 & conceptId == 8532 ~ "Gender: Female",
      # race
      analysisId == 4 & conceptId == 8515 ~ "Race: Asian",
      analysisId == 4 & conceptId == 8516 ~ "Race: Black or African American",
      analysisId == 4 & conceptId == 8527 ~ "Race: White",
      # ethnicity
      analysisId == 5 & conceptId == 38003563 ~ "Ethnicity: Hispanic or Latino",
      analysisId == 5 & conceptId == 38003564 ~ "Ethnicity: Not Hispanic or Latino",
      # age group 5 yr
      analysisId == 3 & name == "age group: -10 -  -6" ~ "Age Group: -10--6",
      analysisId == 3 & name == "age group:  -5 -  -1" ~ "Age Group: -5--1",
      analysisId == 3 & name == "age group:   0 -   4" ~ "Age Group: 0-4",
      analysisId == 3 & name == "age group:   5 -   9" ~ "Age Group: 5-9",
      analysisId == 3 & name == "age group:  10 -  14" ~ "Age Group: 10-14",
      analysisId == 3 & name == "age group:  15 -  19" ~ "Age Group: 15-19",
      analysisId == 3 & name == "age group:  20 -  24" ~ "Age Group: 20-24",
      analysisId == 3 & name == "age group:  25 -  29" ~ "Age Group: 25-29",
      analysisId == 3 & name == "age group:  30 -  34" ~ "Age Group: 30-34",
      analysisId == 3 & name == "age group:  35 -  39" ~ "Age Group: 35-39",
      analysisId == 3 & name == "age group:  40 -  44" ~ "Age Group: 40-44",
      analysisId == 3 & name == "age group:  45 -  49" ~ "Age Group: 45-49",
      analysisId == 3 & name == "age group:  50 -  54" ~ "Age Group: 50-54",
      analysisId == 3 & name == "age group:  55 -  59" ~ "Age Group: 55-59",
      analysisId == 3 & name == "age group:  60 -  64" ~ "Age Group: 60-64",
      analysisId == 3 & name == "age group:  65 -  69" ~ "Age Group: 65-69",
      analysisId == 3 & name == "age group:  70 -  74" ~ "Age Group: 70-74",
      analysisId == 3 & name == "age group:  75 -  79" ~ "Age Group: 75-79",
      analysisId == 3 & name == "age group:  80 -  84" ~ "Age Group: 80-84",
      analysisId == 3 & name == "age group:  85 -  89" ~ "Age Group: 85-89",
      analysisId == 3 & name == "age group:  90 -  94" ~ "Age Group: 90-94",
      analysisId == 3 & name == "age group:  95 -  99" ~ "Age Group: 95-99",
      analysisId == 3 & name == "age group: 100 - 104" ~ "Age Group: 100-104",
      analysisId == 3 & name == "age group: 105 - 109" ~ "Age Group: 105-109",
      analysisId == 3 & name == "age group: 110 - 114" ~ "Age Group: 110-114",
      # index year
      analysisId == 6 & name == "index year: 2000" ~ "Year: 2000",
      analysisId == 6 & name == "index year: 2001" ~ "Year: 2001",
      analysisId == 6 & name == "index year: 2002" ~ "Year: 2002",
      analysisId == 6 & name == "index year: 2003" ~ "Year: 2003",
      analysisId == 6 & name == "index year: 2004" ~ "Year: 2004",
      analysisId == 6 & name == "index year: 2005" ~ "Year: 2005",
      analysisId == 6 & name == "index year: 2006" ~ "Year: 2006",
      analysisId == 6 & name == "index year: 2007" ~ "Year: 2007",
      analysisId == 6 & name == "index year: 2008" ~ "Year: 2008",
      analysisId == 6 & name == "index year: 2009" ~ "Year: 2009",
      analysisId == 6 & name == "index year: 2010" ~ "Year: 2010",
      analysisId == 6 & name == "index year: 2011" ~ "Year: 2011",
      analysisId == 6 & name == "index year: 2012" ~ "Year: 2012",
      analysisId == 6 & name == "index year: 2013" ~ "Year: 2013",
      analysisId == 6 & name == "index year: 2014" ~ "Year: 2014",
      analysisId == 6 & name == "index year: 2015" ~ "Year: 2015",
      analysisId == 6 & name == "index year: 2016" ~ "Year: 2016",
      analysisId == 6 & name == "index year: 2017" ~ "Year: 2017",
      analysisId == 6 & name == "index year: 2018" ~ "Year: 2018",
      analysisId == 6 & name == "index year: 2019" ~ "Year: 2019",
      analysisId == 6 & name == "index year: 2020" ~ "Year: 2020",
      analysisId == 6 & name == "index year: 2021" ~ "Year: 2021",
      analysisId == 6 & name == "index year: 2022" ~ "Year: 2022"
    ),
    id = dplyr::case_when(
      # age group 5 yr
      analysisId == 3 & name == "age group: -10 -  -6" ~ 301,
      analysisId == 3 & name == "age group:  -5 -  -1" ~ 302,
      analysisId == 3 & name == "age group:   0 -   4" ~ 303,
      analysisId == 3 & name == "age group:   5 -   9" ~ 304,
      analysisId == 3 & name == "age group:  10 -  14" ~ 305,
      analysisId == 3 & name == "age group:  15 -  19" ~ 306,
      analysisId == 3 & name == "age group:  20 -  24" ~ 307,
      analysisId == 3 & name == "age group:  25 -  29" ~ 308,
      analysisId == 3 & name == "age group:  30 -  34" ~ 309,
      analysisId == 3 & name == "age group:  35 -  39" ~ 310,
      analysisId == 3 & name == "age group:  40 -  44" ~ 311,
      analysisId == 3 & name == "age group:  45 -  49" ~ 312,
      analysisId == 3 & name == "age group:  50 -  54" ~ 313,
      analysisId == 3 & name == "age group:  55 -  59" ~ 314,
      analysisId == 3 & name == "age group:  60 -  64" ~ 315,
      analysisId == 3 & name == "age group:  65 -  69" ~ 316,
      analysisId == 3 & name == "age group:  70 -  74" ~ 317,
      analysisId == 3 & name == "age group:  75 -  79" ~ 318,
      analysisId == 3 & name == "age group:  80 -  84" ~ 319,
      analysisId == 3 & name == "age group:  85 -  89" ~ 320,
      analysisId == 3 & name == "age group:  90 -  94" ~ 321,
      analysisId == 3 & name == "age group:  95 -  99" ~ 322,
      analysisId == 3 & name == "age group: 100 - 104" ~ 323,
      analysisId == 3 & name == "age group: 105 - 109" ~ 324,
      analysisId == 3 & name == "age group: 110 - 114" ~ 325,
      # index year
      analysisId == 6 & name == "index year: 2000" ~ 62000,
      analysisId == 6 & name == "index year: 2001" ~ 62001,
      analysisId == 6 & name == "index year: 2002" ~ 62002,
      analysisId == 6 & name == "index year: 2003" ~ 62003,
      analysisId == 6 & name == "index year: 2004" ~ 62004,
      analysisId == 6 & name == "index year: 2005" ~ 62005,
      analysisId == 6 & name == "index year: 2006" ~ 62006,
      analysisId == 6 & name == "index year: 2007" ~ 62007,
      analysisId == 6 & name == "index year: 2008" ~ 62008,
      analysisId == 6 & name == "index year: 2009" ~ 62009,
      analysisId == 6 & name == "index year: 2010" ~ 62010,
      analysisId == 6 & name == "index year: 2011" ~ 62011,
      analysisId == 6 & name == "index year: 2012" ~ 62012,
      analysisId == 6 & name == "index year: 2013" ~ 62013,
      analysisId == 6 & name == "index year: 2014" ~ 62014,
      analysisId == 6 & name == "index year: 2015" ~ 62015,
      analysisId == 6 & name == "index year: 2016" ~ 62016,
      analysisId == 6 & name == "index year: 2017" ~ 62017,
      analysisId == 6 & name == "index year: 2018" ~ 62018,
      analysisId == 6 & name == "index year: 2019" ~ 62019,
      analysisId == 6 & name == "index year: 2020" ~ 62020,
      analysisId == 6 & name == "index year: 2021" ~ 62021,
      analysisId == 6 & name == "index year: 2022" ~ 62022,
      TRUE ~ conceptId
    )
  )  %>%
  dplyr::left_join(
    allCohorts, by = c("cohortDefinitionId" ="id"), relationship = "many-to-many"
  ) %>%
  dplyr::select(
    databaseId, cohortDefinitionId, cohortName, id, Covariate,  n, pct
  ) %>%
  dplyr::arrange(
    databaseId, cohortDefinitionId, id
  )


readr::write_csv(demo2, file = fs::path(appDataPath, "baselineDemographics.csv"))


## 4. Baseline Continuous ----------------

cts <- bindCsv(allPaths = allPaths,
               task = listOfTasks[5], # baseline char
               file = "continuous_baseline.csv")


cts2 <- cts %>%
  dplyr::left_join(
    allCohorts, by = c("cohortDefinitionId" ="id"), relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    iqr = p75Value - p25Value,
    name = stringr::str_to_title(name)
  ) %>%
  dplyr::select(
    databaseId, cohortDefinitionId, cohortName, covariateId, name, medianValue, iqr
  ) %>%
  dplyr::arrange(
    databaseId, cohortDefinitionId, covariateId
  )

readr::write_csv(cts2, file = fs::path(appDataPath, "baselineContinuous.csv"))


## 5. Baseline Concepts ----------------

### Extract drug concepts
drug <- bindCsv(allPaths = allPaths,
                task = listOfTasks[5], # baseline char
                file = "drugs_baseline.csv") %>%
  dplyr::filter(pct >= 0.02) %>%
  dplyr::left_join(
    allCohorts, by = c("cohortDefinitionId" ="id"), relationship = "many-to-many"
  )  %>%
  dplyr::mutate(
    domain = "Drugs"
  ) %>%
  dplyr::select(
    databaseId, domain, cohortDefinitionId, cohortName, conceptId, name,  n, pct
  )

### Extract condition concepts
cond <- bindCsv(allPaths = allPaths,
                task = listOfTasks[5], # baseline char
                file = "conditions_baseline.csv") %>%
  dplyr::filter(pct >= 0.02) %>%
  dplyr::left_join(
    allCohorts, by = c("cohortDefinitionId" ="id"), relationship = "many-to-many"
  )  %>%
  dplyr::mutate(
    domain = "Conditions"
  ) %>%
  dplyr::select(
    databaseId, domain, cohortDefinitionId, cohortName, conceptId, name,  n, pct
  )

### Extract procedure concepts (exclude THINBE procedures)
procPaths <- allPaths %>%
  dplyr::filter(listOfDatabase != "THINBE")

proc <- bindCsv(allPaths = procPaths,
                task = listOfTasks[5], # baseline char
                file = "procedures_baseline.csv") %>%
  dplyr::filter(pct >= 0.02) %>%
  dplyr::left_join(
    allCohorts, by = c("cohortDefinitionId" ="id"), relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    domain = "Procedures"
  ) %>%
  dplyr::select(
    databaseId, domain, cohortDefinitionId, cohortName, conceptId, name,  n, pct
  )

conceptTab <- dplyr::bind_rows(
  drug, cond, proc
) %>% dplyr::arrange(databaseId, cohortDefinitionId, domain, conceptId)


readr::write_csv(conceptTab, file = fs::path(appDataPath, "baselineConcepts.csv"))


## 6. Baseline Cohorts ----------------

cohort365 <- bindCsv(allPaths = allPaths,
                     task = listOfTasks[5], # baseline char
                     file = "cohort_covariates_365_0.csv") %>%
  dplyr::left_join(
    cohortManifest %>% dplyr::select(databaseId, id, name),
    by = c("databaseId" = "databaseId", "covariateId" = "id")
  ) %>%
  dplyr::mutate(
    covariateName = name,
    timeWindow = "[-365 : 0]"
    ) %>%
  dplyr::select(databaseId, timeWindow, cohortId, cohortName,
                covariateId, covariateName, count, pct) %>%
  dplyr::arrange(databaseId, cohortId, covariateId)

readr::write_csv(cohort365, file = fs::path(appDataPath, "baselineCohorts.csv"))


## 7. ICD10 Chapters Baseline ----------------

allCohorts2 <- dplyr::bind_rows(
  cohortManifest %>%
    dplyr::rename(cohortName = name,
                  n = subjects) %>%
    dplyr::select(databaseId, id, cohortName, n) %>%
    dplyr::distinct(),
  strataManifest %>%
    dplyr::rename(id = cohort_definition_id,
                  cohortName = name) %>%
    dplyr::select(databaseId, id, cohortName, n) %>%
    dplyr::distinct()
)

### Extract ICD10
icd10 <- bindCsv(allPaths = allPaths,
                 task = listOfTasks[5], # baseline char
                 file = "condition_chapters.csv") %>%
  dplyr::filter(CATEGORY_CODE != 0) %>%
  dplyr::left_join(
    allCohorts2, by = c("COHORT_ID" ="id", "databaseId"), relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    categoryName = dplyr::case_when(
      CATEGORY_CODE == 443723 ~ glue::glue("{CATEGORY_NAME}: Disorder of cellular component of blood"),
      CATEGORY_CODE == 440371 ~ glue::glue("{CATEGORY_NAME}: Disorder of immune function"),
      CATEGORY_CODE == 432795 ~ glue::glue("{CATEGORY_NAME}: Traumatic or non-traumatic injury"),
      CATEGORY_CODE == 444363 ~ glue::glue("{CATEGORY_NAME}: Drug-related disorder"),
      CATEGORY_CODE == 442562 ~ glue::glue("{CATEGORY_NAME}: Poisoning"),
      CATEGORY_CODE == 4088927 ~ glue::glue("{CATEGORY_NAME}: Pregnancy, childbirth and puerperium finding"),
      CATEGORY_CODE == 4154314 ~ glue::glue("{CATEGORY_NAME}: Finding of arrangement of fetus"),
      CATEGORY_CODE == 435875 ~ glue::glue("{CATEGORY_NAME}: Complication of pregnancy, childbirth and/or puerperium"),
      CATEGORY_CODE == 4136529 ~ glue::glue("{CATEGORY_NAME}: Fetal movement finding"),
      CATEGORY_CODE == 441406 ~ glue::glue("{CATEGORY_NAME}: Disorder of fetus or newborn"),
      CATEGORY_CODE == 432250 ~ glue::glue("{CATEGORY_NAME}: Disorder due to infection"),
      CATEGORY_CODE == 438112 ~ glue::glue("{CATEGORY_NAME}: Neoplastic disease"),
      CATEGORY_CODE == 31821 ~ glue::glue("{CATEGORY_NAME}: Disorder of endocrine system"),
      CATEGORY_CODE == 436670 ~ glue::glue("{CATEGORY_NAME}: Metabolic disease"),
      CATEGORY_CODE == 4090739 ~ glue::glue("{CATEGORY_NAME}: Nutritional disorder"),
      CATEGORY_CODE == 432586 ~ glue::glue("{CATEGORY_NAME}: Mental disorder"),
      CATEGORY_CODE == 4011630 ~ glue::glue("{CATEGORY_NAME}: Neurological finding"),
      CATEGORY_CODE == 376337 ~ glue::glue("{CATEGORY_NAME}: Disorder of nervous system"),
      CATEGORY_CODE == 4038502 ~ glue::glue("{CATEGORY_NAME}: Eye / vision finding"),
      CATEGORY_CODE == 4042836 ~ glue::glue("{CATEGORY_NAME}: Disorder of head"),
      CATEGORY_CODE == 134057 ~ glue::glue("{CATEGORY_NAME}: Disorder of cardiovascular system"),
      CATEGORY_CODE == 320136 ~ glue::glue("{CATEGORY_NAME}: Disorder of respiratory system"),
      CATEGORY_CODE == 4302537 ~ glue::glue("{CATEGORY_NAME}: Digestive system finding"),
      CATEGORY_CODE == 4028387 ~ glue::glue("{CATEGORY_NAME}: Disorder of integument"),
      CATEGORY_CODE == 4244662 ~ glue::glue("{CATEGORY_NAME}: Disorder of musculoskeletal system"),
      CATEGORY_CODE == 4027384 ~ glue::glue("{CATEGORY_NAME}: Inflammatory disorder"),
      CATEGORY_CODE == 40482430 ~ glue::glue("{CATEGORY_NAME}: Deformity of limb"),
      CATEGORY_CODE == 4344497 ~ glue::glue("{CATEGORY_NAME}: Soft tissue lesion"),
      CATEGORY_CODE == 433595 ~ glue::glue("{CATEGORY_NAME}: Edema"),
      CATEGORY_CODE == 4041285 ~ glue::glue("{CATEGORY_NAME}: Urogenital finding"),
      CATEGORY_CODE == 4053838 ~ glue::glue("{CATEGORY_NAME}: Foreign body"),
      CATEGORY_CODE == 4105886 ~ glue::glue("{CATEGORY_NAME}: Adverse reaction"),
      CATEGORY_CODE == 440508 ~ glue::glue("{CATEGORY_NAME}: Congenital disease")
    )
  ) %>%
  dplyr::arrange(databaseId, COHORT_ID, CATEGORY_ID) %>%
  dplyr::mutate(
    pct = COUNTVALUE / n
  ) %>%
  dplyr::select(databaseId, COHORT_ID, cohortName,
                CATEGORY_CODE, categoryName, COUNTVALUE, pct)

readr::write_csv(icd10, file = fs::path(appDataPath, "baselineChapters.csv"))


## 8. Post Index Prevalence ----------------

### A. Post Index Conditions ----------------

piPrevFilesCond <- c("cohort_covariates_1_365.csv",
                     "cohort_covariates_366_730.csv",
                     "cohort_covariates_731_1825.csv")
piPrevTimeFrameCond <- c("1d - 365d",
                         "366d - 730d",
                         "731d - 1825d")
piPrevCond <- purrr::map2_dfr(piPrevFilesCond,         # files to use
                              piPrevTimeFrameCond,     # time frame column to add
                              ~bindCsv(                # bind csv
                                allPaths = allPaths,
                                task = listOfTasks[6], # postindex Conditions
                                file = .x) %>%
                                dplyr::mutate(         # add timeWindow Column
                                  timeWindow = .y
                                ))  %>%
  dplyr::mutate(
    type = "conditions"
  ) %>%
  dplyr::select(-covariateName) %>%
  dplyr::left_join(
    cohortManifest %>% dplyr::select(databaseId, id, name),
    by = c("databaseId" = "databaseId", "covariateId" = "id")
  ) %>%
  dplyr::filter(
    name %in% c("adenomyosis", "coagulopathy", "disorderOfOvary",
                "dysmenorrhea", "endoHyperplasia", "endoPolyp", "endometriosis",
                "ovulatoryDysfunction", "pcos", "uterineLeiomyoma")
  ) %>%
  dplyr::rename(covariateName = name)


### B. Post Index Drugs ----------------

piPrevFilesDrugs <- c(
  "cohort_covariates_1_183.csv",
  "cohort_covariates_184_365.csv",
  "cohort_covariates_1_365.csv",
  "cohort_covariates_366_730.csv",
  "cohort_covariates_731_1825.csv"
)

piPrevTimeFrameDrugs <- c(
  "1d - 183d",
  "184d - 365d",
  "1d - 365d",
  "366d - 730d",
  "731d - 1825d"
)

piPrevDrugs <- purrr::map2_dfr(piPrevFilesDrugs,        # files to use
                               piPrevTimeFrameDrugs,    # time frame column to add
                               ~bindCsv(                # bind csv
                                 allPaths = allPaths,
                                 task = listOfTasks[7], # postindex Drugs
                                 file = .x) %>%
                                 dplyr::mutate(         # add timeWindow Column
                                   timeWindow = .y
                                 )) %>%
  dplyr::mutate(
    type = "drugs"
  )


### C. Post Index Procedures ----------------
# skip THIN no procedures

piPrevFilesProc <- c("procedure_prevalence_1.csv",
                     "procedure_prevalence_1001.csv",
                     "procedure_prevalence_1002.csv",
                     "procedure_prevalence_1003.csv")

piPrevProcCohorts <- c(1L, 1001L, 1002L, 1003L)

piPrevProc <- purrr::map2_dfr(piPrevFilesProc,          # files to use
                              piPrevProcCohorts,        # cohorts
                              ~bindCsv(                 # bind csv
                                allPaths = procPaths,   # skip THIN no procedures
                                task = listOfTasks[11], # postindex Proc
                                file = .x) %>%
                                dplyr::mutate(
                                  cohortId = .y
                                )) %>%
  dplyr::mutate(
    cohortName = dplyr::case_when(
      cohortId == 1 ~ "hmb",
      cohortId == 1001L ~ "hmb_age_lt_30",
      cohortId == 1002L ~ "hmb_age_30_45",
      cohortId == 1003L ~ "hmb_age_45_55"
    ),
    covariateId = cohortDefinitionId,
    covariateName = dplyr::case_when(
      covariateId == 36 ~ "bloodTransfusion",
      covariateId == 37 ~ "copperIUDprocedure",
      covariateId == 38 ~ "endometrialAblation",
      covariateId == 39 ~ "hormonalIUD",
      covariateId == 40 ~ "hysterectomy",
      covariateId == 41 ~ "myomectomy",
      covariateId == 42 ~ "uae",
      covariateId == 43 ~ "undefinedIUD"
    ),
    count = numEvents,
    timeWindow = dplyr::case_when(
      window == "All" ~ "1d - 9999d",
      window == "1 - 183" ~ "1d - 183d",
      window == "1 - 365" ~ "1d - 365d",
      window == "184 - 365" ~ "184d - 365d",
      window == "366 - 730" ~ "366d - 730d",
      window == "731 - 1825" ~ "731d - 1825d"
    ),
    type = "procedures"
  ) %>%
  dplyr::select(databaseId, cohortId, cohortName, covariateId, covariateName,
                count, pct, timeWindow, type)

postIndexPrev <- dplyr::bind_rows(
  piPrevCond, piPrevDrugs, piPrevProc
)

readr::write_csv(postIndexPrev, file = fs::path(appDataPath, "postIndexPrevalence.csv"))


## 9. Incidence ----------------

inicFiles <- glue::glue("incidence_analysis_ref_{1:4}.csv")

inic <- purrr::map_dfr(inicFiles,               # files to use
                       ~bindCsv(                # bind csv
                         allPaths = allPaths,
                         task = listOfTasks[4],
                         file = .x))

fctOrder <- c("All", as.character(2000:2022))

inic2 <- inic %>%
  dplyr::mutate(
    START_YEAR = ifelse(is.na(START_YEAR), "All", as.character(START_YEAR)),
    START_YEAR = factor(START_YEAR, levels = fctOrder),
    INCIDENCE_RATE_P1000PY = INCIDENCE_RATE_P100PY * 10
  ) %>%
  dplyr::select(databaseId,
                START_YEAR,
                OUTCOME_COHORT_DEFINITION_ID, OUTCOME_NAME,
                PERSONS_AT_RISK, PERSON_DAYS, OUTCOMES,
                INCIDENCE_PROPORTION_P100P,
                INCIDENCE_RATE_P1000PY) %>%
  dplyr::arrange(databaseId, OUTCOME_COHORT_DEFINITION_ID, START_YEAR)

 readr::write_csv(inic2, file = fs::path(appDataPath, "incidence.csv"))


## 10. Treatment Patterns ----------------

### A. HMB normal ----------------

txPath <- allPaths %>%
  dplyr::filter(listOfTasks == listOfTasks[9])

### Get treatment patterns table
txPathDat <- purrr::pmap_dfr(
  txPath,
  ~bindTxPathTab(path = ..3, database = ..1)
) %>%
  tidyr::separate_wider_delim(
    cols = cohortName,
    delim = "_",
    names = c("type", "cohortId")
  ) %>%
  dplyr::mutate(
    cohortName = dplyr::case_when(
      cohortId == 1 ~ "hmb",
      cohortId == 1001L ~ "hmb_age_lt_30",
      cohortId == 1002L ~ "hmb_age_30_45",
      cohortId == 1003L ~ "hmb_age_45_55"
    )
  ) %>%
  dplyr::select(databaseId, cohortId, cohortName, event_cohort_name1:event_cohort_name5, End, n) %>%
  dplyr::arrange(databaseId, cohortId, desc(n))

readr::write_csv(txPathDat, file = fs::path(appDataPath, "treatmentPatterns.csv"))


### B. HMB 2 ----------------
txPath2 <- allPaths %>%
  dplyr::filter(listOfTasks == listOfTasks[14])

### Get treatment patterns table
txPathDat2 <- purrr::pmap_dfr(
  txPath2,
  ~bindTxPathTab(path = ..3, database = ..1)
) %>%
  tidyr::separate_wider_delim(
    cols = cohortName,
    delim = "_",
    names = c("type", "cohortId")
  ) %>%
  dplyr::mutate(
    cohortName = dplyr::case_when(
      cohortId == 44 ~ "hmb2",
      cohortId == 44001L ~ "hmb2_age_lt_30",
      cohortId == 44002L ~ "hmb2_age_30_45",
      cohortId == 44003L ~ "hmb2_age_45_55"
    )
  ) %>%
  dplyr::select(databaseId, cohortId, cohortName, event_cohort_name1:event_cohort_name5, End, n) %>%
  dplyr::arrange(databaseId, cohortId, desc(n))

readr::write_csv(txPathDat2, file = fs::path(appDataPath, "treatmentPatterns2.csv"))


## get sankey diagram
# sankey <- purrr::pmap(
#   txPath,
#   ~groupSankey(path = ..3, database = ..1)
# )
# names(sankey) <- listOfDatabase
# readr::write_rds(sankey, file = fs::path(appDataPath, "sankey.rds"))


## 11. Time to event ----------------

### A. Time to discontinuation ----------------

### List files to extract
ttdFiles <- c("tte_1.csv",
              "tte_1001.csv",
              "tte_1002.csv",
              "tte_1003.csv")

permutations <- tidyr::expand_grid(
  ttdFiles,
  listOfDatabase
)

### Bind all in ttd
ttd <- purrr::pmap_dfr(
  permutations,
  ~bindTteData(
    path = resultsPath,
    database = ..2,
    task = listOfTasks[10],
    file = ..1
  )
)

arrow::write_parquet(
  x = ttd,
  sink = fs::path(appDataPath, "ttd.parquet")
)


### B. Time to intervention ----------------

ttiFiles <- c("procedure_survival_1.csv",
              "procedure_survival_1001.csv",
              "procedure_survival_1002.csv",
              "procedure_survival_1003.csv")

permutations <- tidyr::expand_grid(
  ttiFiles,
  listOfDatabase
)

### Bind all in ttd
tti <- purrr::pmap_dfr(
  permutations,
  ~bindTteData2(
    path = resultsPath,
    database = ..2,
    task = listOfTasks[12],
    file = ..1
  )
)

arrow::write_parquet(
  x = tti,
  sink = fs::path(appDataPath, "tti.parquet")
)

