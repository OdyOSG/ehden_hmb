# Load data into environment for app

# Dependencies -----------------

library(dplyr)
library(readr)

source(here::here("shiny", "R", "fn.R"))
source(here::here("shiny", "migration", "helpers.R"))

dataPath <- here::here("shiny", "data")

# 1. Cohorts -----------------
## Load cohort Counts
cohortCounts <- readr::read_csv(fs::path(dataPath, "cohortCounts.csv"),
                                show_col_types = FALSE)

cohortCounts2 <- readr::read_csv(fs::path(dataPath, "cohortCounts2.csv"),
                                show_col_types = FALSE)

## Load strata counts
strataCounts <- readr::read_csv(fs::path(dataPath, "strataCounts.csv"),
                                show_col_types = FALSE)

strataCounts$`Strata Cohort Name` <- gsub("hmb age_lt_30", "hmb age_11_29", strataCounts$`Strata Cohort Name`)
strataCounts$`Strata Cohort Name` <- gsub("hmb age_30_45", "hmb age_30_44", strataCounts$`Strata Cohort Name`)

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

demoChar$cohortName <- gsub("hmb age_lt_30", "hmb age_11_29", demoChar$cohortName)
demoChar$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", demoChar$cohortName)

## Pickers
cohortNameDemo <- unique(demoChar$cohortName)


## Continuous baseline
ctsChar <- readr::read_csv(fs::path(dataPath, "baselineContinuous.csv"),
                           show_col_types = FALSE) %>%
  dplyr::select(-cohortDefinitionId)

ctsChar$cohortName <- gsub("hmb age_lt_30", "hmb age_11_29", ctsChar$cohortName)
ctsChar$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", ctsChar$cohortName)

## Pickers
cohortNameCts <- unique(ctsChar$cohortName)

## Concept baseline
conceptChar <- readr::read_csv(fs::path(dataPath, "baselineConcepts.csv"),
                               show_col_types = FALSE) %>%
  dplyr::rename(count = n) %>%
  dplyr::select(-cohortDefinitionId, -timeWindow) %>%
  maskLowCount()

conceptChar$cohortName <- gsub("hmb age_lt_30", "hmb age_11_29", conceptChar$cohortName)
conceptChar$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", conceptChar$cohortName)

## Pickers
cohortNameConcept <- unique(conceptChar$cohortName)

## Cohort baseline
cohortChar <- readr::read_csv(fs::path(dataPath, "baselineCohorts.csv"),
                              show_col_types = FALSE) %>%
  dplyr::rename(covariateName2 = covariateName) %>%
  dplyr::mutate(
    domain = dplyr::case_when(
      covariateName2 %in% c("antidepressants", "antipsychotics", "antithrombotics", "nsaids", "grha", "tranexamicAcid",
                           "tamoxifen", "gonadalSteroids", "copperIUDdrug", "danazol", "hormonalLngIUD", "ulipristalAcetate",
                           "oc_estradiolDienogest", "oc_other", "progestinOnly", "ironPreparations",
                           "oralContraceptives_estradiolDienogest", "oralContraceptives_other") ~ "Drugs",
      covariateName2 %in% c("copperIUDprocedure", "bloodTransfusion", "hysterectomy", "myomectomy",
                           "uae", "undefinedIUD", "endometrialAblation") ~ "Procedures",
      TRUE ~ "Conditions"
    )
  ) %>%
  dplyr::select(databaseId, cohortName, domain, covariateName2, count, pct) %>%
  maskLowCount()

cohortChar$covariateName <- gsub("oralContraceptives_estradiolDienogest", "oc_estradiolDienogest", cohortChar$covariateName2)
cohortChar$covariateName <- gsub("oralContraceptives_other", "oc_other", cohortChar$covariateName)
cohortChar$covariateName2 <- NULL
cohortChar <- cohortChar %>% dplyr::select(databaseId, cohortName, domain, covariateName, count, pct)

cohortChar$cohortName <- gsub("hmb age_lt_30", "hmb age_11_29", cohortChar$cohortName)
cohortChar$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", cohortChar$cohortName)

## Pickers
cohortNameCohort <- unique(cohortChar$cohortName)

## Chapters baseline
icdChar <- readr::read_csv(fs::path(dataPath, "baselineChapters.csv"),
                           show_col_types = FALSE) %>%
  dplyr::select(-COHORT_ID, -timeWindow) %>%
  dplyr::rename(count = COUNTVALUE) %>%
  maskLowCount()

icdChar$cohortName <- gsub("hmb age_lt_30", "hmb age_11_29", icdChar$cohortName)
icdChar$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", icdChar$cohortName)

## Pickers
cohortNameICD <- unique(icdChar$cohortName)


### Baseline Pickers
domainConceptChar <- sort(unique(conceptChar$domain))
domainCohortChar  <- sort(unique(cohortChar$domain))


# 3. Incidence (IncidencePrevalence) -----------------

## Incidence 2
incTab2 <- readr::read_csv(fs::path(dataPath, "incidence.csv"),
                           show_col_types = FALSE) %>%
  dplyr::select(-c(cohort_obscured, result_obscured, outcome_cohort_id, denominator_cohort_id,
                   analysis_repeated_events, analysis_outcome_washout, analysis_complete_database_intervals,
                   denominator_cohort_name, denominator_sex, denominator_days_prior_observation,
                   denominator_target_cohort_definition_id, denominator_target_cohort_name,
                   analysis_min_cell_count, analysis_id, denominator_start_date, denominator_end_date)) %>%
  dplyr::mutate(n_events = dplyr::if_else(n_events <=5, -5, n_events, 0)) %>%
  dplyr::mutate(
    incidenceYear = dplyr::case_when(
      analysis_interval == "years"~ as.character(format(incidence_start_date, "%Y")),
      analysis_interval == "overall"~ "All",
      TRUE ~ NA),
    ageGroup = dplyr::case_when(
      denominator_age_group == "0 to 130" ~ "Total",
      TRUE ~ denominator_age_group
    )
  ) %>%
  dplyr::select(-c(analysis_interval, denominator_age_group, incidence_start_date, incidence_end_date)) %>%
  dplyr::mutate(
    databaseName = dplyr::case_when(
      cdm_name == "CPRD_EHR_AURUM_OMOP" ~ "cprdAurum",
      cdm_name == "CPRD_EHR_GOLD_OMOP" ~ "cprdGold",
      cdm_name == "MKTSCAN_CLAIMS_OMOP" ~ "mrktscan",
      cdm_name == "OPTUM_CLAIMS_OMOP" ~ "optum",
      cdm_name == "Institut Municipal Assistència Sanitària Information System" ~ "IMASIS",
      TRUE ~ cdm_name
    ),
    incidence_1000_pys = dplyr::if_else(
      n_events == -5, "-5", format(round(incidence_100000_pys/100, 2), big.mark = ",", scientific = FALSE), "-5"),
    incidence_1000_pys_95CI_lower = dplyr::if_else(
      n_events == -5, "-5", format(round(incidence_100000_pys_95CI_lower/100, 2), big.mark = ",", scientific = FALSE), "-5"),
    incidence_1000_pys_95CI_upper = dplyr::if_else(
      n_events == -5, "-5", format(round(incidence_100000_pys_95CI_upper/100, 2), big.mark = ",", scientific = FALSE), "-5"),
    person_years = format(round(person_years), big.mark = ",", scientific = FALSE),
    person_days = format(person_days, big.mark = ",", scientific = FALSE),
    incidenceProp_100_ps = dplyr::if_else(
      n_events == -5, -5, round((n_events/n_persons) * 100, 2), -5)
  ) %>%
  dplyr::mutate(
    n_events = format(n_events, big.mark = ",", scientific = FALSE),
    n_persons = format(n_persons, big.mark = ",", scientific = FALSE)
  ) %>%
  dplyr::select(-c(incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper, cdm_name,
                   incidence_1000_pys_95CI_lower, incidence_1000_pys_95CI_upper)) %>%
  dplyr::select(databaseName, incidenceYear, ageGroup, outcome_cohort_name, n_persons,
                person_years, person_days, n_events, incidenceProp_100_ps, incidence_1000_pys)

incTab2$ageGroup <- gsub("0 to 29", "11 to 29", incTab2$ageGroup)

### Incidence 2 Pickers

## Table
databaseInci2   <- unique(incTab2$databaseName)
yearInci2       <- unique(incTab2$incidenceYear)
ageInci2        <- unique(incTab2$ageGroup) %>% sort(decreasing = TRUE)
cohortNameInci2 <- "hmb"

## Plot
cohortNameInci2Plot <- "hmb"
ageInci2Plot        <- unique(incTab2$ageGroup) %>% sort(decreasing = TRUE)
databaseInciPlot2   <- unique(incTab2$databaseName)


# 4. Post-Index Prevalence -----------------

postIndex <- readr::read_csv(fs::path(dataPath, "postIndexPrevalence.csv"),
                             show_col_types = FALSE) %>%
  dplyr::select(-cohortId, -covariateId) %>%
  maskLowCount()

postIndex$cohortName <- gsub("hmb age_lt_30", "hmb age_11_29", postIndex$cohortName)
postIndex$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", postIndex$cohortName)

## Underlying conditions -----------------

condPi <- postIndex %>%
  dplyr::filter(type == "conditions") %>%
  dplyr::select(databaseId, timeWindow, cohortName, covariateName, count, pct) %>%
  dplyr::arrange(databaseId, timeWindow)

### Pickers
condCohort <- unique(condPi$cohortName)
condCov <- unique(condPi$covariateName)
condTimeWindow <- unique(condPi$timeWindow)


## Drug Utilization -----------------

drugPi <- postIndex %>%
  dplyr::filter(type == "drugs") %>%
  dplyr::select(databaseId, timeWindow, cohortName, covariateName, count, pct, cat) %>%
  dplyr::arrange(databaseId, timeWindow) %>%
  dplyr::mutate(timeWindow = as.character(timeWindow),
                cat = dplyr::case_when(
                  cat == "within" ~ "Within time window",
                  cat == "followUp" ~ "Complete follow-up"
                  )
                )

### Pickers
drugCohort <- unique(drugPi$cohortName)
drugCov <- unique(drugPi$covariateName)
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

txPatDatAll$cohortName <-  gsub("hmb_age_lt_30", "hmb_age_11_29", txPatDatAll$cohortName)
txPatDatAll$cohortName <- gsub("hmb age_30_45", "hmb age_30_44", txPatDatAll$cohortName)

### Sankey pickers
cohortName2 <- c("hmb", "hmb_age_11_29", "hmb age_30_44", "hmb_age_45_55")

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

ttd$`Cohort Name` <- gsub("hmb age_lt_30", "hmb age_11_29", ttd$`Cohort Name`)
ttd$`Cohort Name` <- gsub("hmb age_30_45", "hmb age_30_44", ttd$`Cohort Name`)


ttd2 <- arrow::read_parquet(file = fs::path(here::here(dataPath ,"ttd2.parquet"))) %>%
  dplyr::mutate(targetId = as.double(targetId)) %>%
  dplyr::left_join(strataCounts, by = c("targetId" = "Strata Cohort Id", "database" = "Database")) %>%
  dplyr::rename(`Cohort Name` = `Strata Cohort Name`) %>%
  dplyr::select(database:`Cohort Name`) %>%
  dplyr::mutate(`Cohort Name` = dplyr::case_when(
    targetId == 1 ~ "hmb",
    TRUE ~ `Cohort Name`)
  )

ttd2$`Cohort Name` <- gsub("hmb age_lt_30", "hmb age_11_29", ttd2$`Cohort Name`)
ttd2$`Cohort Name` <- gsub("hmb age_30_45", "hmb age_30_44", ttd2$`Cohort Name`)


### TTE cohort pickers
ttdCohorts <- unique(ttd$`Cohort Name`)

cohortName3 <- c("hmb", "hmb age_11_29", "hmb age_30_44", "hmb age_45_55")

### TTE pickers
tteCohorts <- tibble::tibble(
  id = c(1, 1001L, 1002L, 1003L),
  name = cohortName3
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

tti$`Cohort Name` <- gsub("hmb age_lt_30", "hmb age_11_29", tti$`Cohort Name`)
tti$`Cohort Name` <- gsub("hmb age_30_45", "hmb age_30_44", tti$`Cohort Name`)

cohortNames <- cohortCounts %>%
  dplyr::select(`Cohort Name`, `Cohort Id`) %>%
  dplyr::distinct()

tti2 <- tti %>%
  dplyr::left_join(cohortNames, by = c("outcomeCohortId" = "Cohort Id")) %>%
  dplyr::select(-outcomeCohortId) %>%
  dplyr::rename(outcomeCohortId = `Cohort Name.y`,
                `Cohort Name` = `Cohort Name.x`)


### TTI cohort pickers
ttiCohorts <- tti %>%
  dplyr::select(targetId, `Cohort Name`) %>%
  dplyr::rename(id = targetId, name = `Cohort Name`) %>%
  dplyr::distinct()


# 7. Age distribution plot -----------------


## Format demographics -------------------

baselineChar <- readr::read_csv(fs::path(dataPath, "baselineDemographics.csv"),
                                show_col_types = FALSE)

baselineChar$name <- gsub("Age Group: ", "", baselineChar$Covariate)

demographics <- baselineChar %>%
  dplyr::filter(id %in% c(300:320),
                cohortName == "hmb") %>%
  dplyr::select(databaseId, cohortName, id, name, n, pct)


### 10-year age groups -------------------

#### Percentage -------------------
age10GrpP <- demographics %>%
  dplyr::mutate(
    name2 = dplyr::case_when(
      id %in% c(305, 306) ~ "11-19",
      id %in% c(307, 308) ~ "20-29",
      id %in% c(309, 310) ~ "30-39",
      id %in% c(311, 312) ~ "40-49",
      id %in% c(313, 314) ~ "50-55"
    )
  ) %>%
  dplyr::group_by(databaseId, cohortName, name2) %>%
  dplyr::summarize(
    count = sum(n), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cohortCounts2, by = c("cohortName" = "Cohort Name", "databaseId" = "Database")
  ) %>%
  dplyr::mutate(
    pct = count / Subjects,
    value = scales::label_percent(accuracy = 0.1, suffix = "")(pct),
    category = "10_yr",
    type = "pct"
  ) %>%
  dplyr::select(databaseId, cohortName, name2, category, pct, value, type) %>%
  dplyr::rename(`Database Name` = databaseId,
                name = name2)


#### Counts -------------------
age10GrpC <- demographics %>%
  dplyr::mutate(
    name2 = dplyr::case_when(
      id %in% c(305, 306) ~ "11-19",
      id %in% c(307, 308) ~ "20-29",
      id %in% c(309, 310) ~ "30-39",
      id %in% c(311, 312) ~ "40-49",
      id %in% c(313, 314) ~ "50-55"
    )
  ) %>%
  dplyr::group_by(databaseId, cohortName, name2) %>%
  dplyr::summarize(
    count = sum(n), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cohortCounts2, by = c("cohortName" = "Cohort Name", "databaseId" = "Database")
  ) %>%
  dplyr::mutate(
    pct = count / Subjects,
    value = scales::label_comma()(count),
    category = "10_yr",
    type = "count"
  ) %>%
  dplyr::select(databaseId, cohortName, name2, category, count, value, type) %>%
  dplyr::rename(`Database Name` = databaseId,
                name = name2)


### 5-year age groups -------------------

#### Percentage -------------------
age5GrpP <- demographics %>%
  dplyr::mutate(
    name = dplyr::case_when(
      name == "10-14" ~ "11-14",
      name == "55-59" ~ "55",
      TRUE ~ name
    )
  ) %>%
  dplyr::group_by(databaseId, cohortName, name) %>%
  dplyr::summarize(
    count = sum(n), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cohortCounts2, by = c("cohortName" = "Cohort Name", "databaseId" = "Database")
  ) %>%
  dplyr::mutate(
    pct = count / Subjects,
    value = scales::label_percent(accuracy = 0.1, suffix = "")(pct),
    category = "5_yr",
    type = "pct"
  ) %>%
  dplyr::select(databaseId, cohortName, name, category, pct, value, type) %>%
  dplyr::rename(`Database Name` = databaseId)


#### Counts -------------------
age5GrpC <- demographics %>%
  dplyr::mutate(
    name = dplyr::case_when(
      name == "10-14" ~ "11-14",
      name == "55-59" ~ "55",
      TRUE ~ name
    )
  ) %>%
  dplyr::group_by(databaseId, cohortName, name) %>%
  dplyr::summarize(
    count = sum(n), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cohortCounts2, by = c("cohortName" = "Cohort Name", "databaseId" = "Database")
  ) %>%
  dplyr::mutate(
    pct = count / Subjects,
    value = scales::label_comma()(count),
    category = "5_yr",
    type = "count"
  ) %>%
  dplyr::select(databaseId, cohortName, name, category, count, value, type) %>%
  dplyr::rename(`Database Name` = databaseId)


### Study's age groups -------------------

#### Percentage -------------------
ageGrpP <- demographics %>%
  dplyr::mutate(
    name2 = dplyr::case_when(
      id %in% c(305, 306, 307, 308) ~ "11-29",
      id %in% c(309, 310, 311) ~ "30-44",
      id %in% c(312, 313, 314) ~ "45-55"
    )
  ) %>%
  dplyr::group_by(databaseId, cohortName, name2) %>%
  dplyr::summarize(
    count = sum(n), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cohortCounts2, by = c("cohortName" = "Cohort Name", "databaseId" = "Database")
  ) %>%
  dplyr::mutate(
    pct = count / Subjects,
    value = scales::label_percent(accuracy = 0.1, suffix = "")(pct),
    category = "study_yr",
    type = "pct"
  ) %>%
  dplyr::select(databaseId, cohortName, name2, category, pct, value, type) %>%
  dplyr::rename(`Database Name` = databaseId,
                name = name2)

#### Counts -------------------
ageGrpC <- demographics %>%
  dplyr::mutate(
    name2 = dplyr::case_when(
      id %in% c(305, 306, 307, 308) ~ "11-29",
      id %in% c(309, 310, 311) ~ "30-44",
      id %in% c(312, 313, 314) ~ "45-55"
    )
  ) %>%
  dplyr::group_by(databaseId, cohortName, name2) %>%
  dplyr::summarize(
    count = sum(n), .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cohortCounts2, by = c("cohortName" = "Cohort Name", "databaseId" = "Database")
  ) %>%
  dplyr::mutate(
    pct = count / Subjects,
    value = scales::label_comma()(count),
    category = "study_yr",
    type = "count"
  ) %>%
  dplyr::select(databaseId, cohortName, name2, category, count, value, type) %>%
  dplyr::rename(`Database Name` = databaseId,
                name = name2)

## Bind all age group data frames together
allAge <- dplyr::bind_rows(age10GrpP, age10GrpC, age5GrpP, age5GrpC, ageGrpP, ageGrpC)


### Pickers -------------
ageDisType <- unique(allAge$type) %>% sort()
names(ageDisType) <- c("Person Count", "Percentage")

ageDisCat <- unique(allAge$category) %>% sort()
names(ageDisCat) <- c("10-year groups", "5-year groups", "Study's groups")

ageDisDb <- unique(allAge$`Database Name`)

