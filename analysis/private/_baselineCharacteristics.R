# A. Meta Info -----------------------

# Task: Baseline Characteristics
# Author: Martin Lavallee
# Date: 2023-07-24
# Description: The purpose of the _baselineCharacteristics.R script is to
# provide internal functions for the baseline characteristics analysis

# B. Functions ------------------------
source("analysis/private/_utilities.R")


# Helpers -----------------
silentCovariates <- function(con, cdmDatabaseSchema, cohortTable, cohortDatabaseSchema, cohortId, covSettings, aggregate = TRUE) {
  cli::cat_bullet("Getting Covariates from database...",
                  bullet = "info", bullet_col = "blue")
  tik <- Sys.time()
  #get covariate data
  quietCov <- purrr::quietly(FeatureExtraction::getDbCovariateData)
  cov <- quietCov(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covariateSettings = covSettings,
    aggregated = aggregate
  )$result
  tok <- Sys.time()
  cli::cat_bullet("Covariates built at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Covariate build took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")
  return(cov)
}

# Domain FE -------------------------

baselineDemographics <- function(con,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 cdmDatabaseSchema,
                                 cohortId,
                                 outputFolder) {

  cli::cat_rule("Build Demographic Covariates")
  # cat_cohortId <- paste(cohortId, collapse = ", ")
  # cli::cat_bullet("Build demographic covariates for cohorts:\n\t", crayon::green(cat_cohortId),
  #                 bullet = "info", bullet_col = "blue")

  # Create Demographic settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE,
    useDemographicsRace = TRUE,
    useDemographicsEthnicity = TRUE,
    useDemographicsIndexYear = TRUE
  )

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # format
  # TODO should we improve the covariateName format
  demoTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect()

  verboseSave(object = demoTbl,
              savename = "demographics_baseline",
              saveLocation = outputFolder)
  invisible(demoTbl)
}


baselineContinuous <- function(con,
                               cohortDatabaseSchema,
                               cohortTable,
                               cdmDatabaseSchema,
                               cohortId,
                               timeA = -365,
                               timeB = -1,
                               outputFolder) {

  cli::cat_rule("Build Continuous Covariates")
  # Create Continuous settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime = TRUE,
    useDemographicsTimeInCohort = TRUE,
    useCharlsonIndex = TRUE,
    useVisitConceptCountLongTerm = TRUE,
    useDcsi = TRUE,
    useChads2Vasc = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # format
  ctsTbl <- cov$covariatesContinuous %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, covariateId, name, minValue, p10Value, p25Value, medianValue, p75Value, p90Value, maxValue) %>%
    dplyr::collect()

  verboseSave(object = ctsTbl,
              savename = "continuous_baseline",
              saveLocation = outputFolder)
  invisible(ctsTbl)
}

baselineDrugs <- function(con,
                          cohortDatabaseSchema,
                          cohortTable,
                          cdmDatabaseSchema,
                          cohortId,
                          timeA = -365,
                          timeB = -1,
                          outputFolder) {

  cli::cat_rule("Build Drug Covariates")
  # cat_cohortId <- paste(cohortId, collapse = ", ")
  # cli::cat_bullet("Build drugs covariates:\n\t", crayon::green(cat_cohortId),
  #                 bullet = "info", bullet_col = "blue")

  # Create Drugs settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDrugGroupEraLongTerm = TRUE,
    excludedCovariateConceptIds = c(21600001, 21600959, 21601237, # Remove ATC 1st class
                                    21601907, 21602359, 21602681,
                                    21602795, 21601386, 21603931,
                                    21604180, 21604847, 21605007,
                                    21603550, 21605212),
    longTermStartDays = timeA,
    endDays = timeB
  )

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # format
  drugTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name)
    )

  #TODO subset only atc2


  verboseSave(object = drugTbl,
              savename = "drugs_baseline",
              saveLocation = outputFolder)
  invisible(drugTbl)

}

conditionGroups <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            timeA = -365,
                            timeB = -1,
                            outputFolder) {

  if (length(cohortId) != 1) {
    stop("Can only run 1 cohort at a time")
  }
  cli::cat_line("\n\n")
  cli::cat_bullet("Building Condition Group Covariates for cohort ", crayon::green(cohortId),
                  bullet = "tick", bullet_col = "green")

  covSettings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings,
                          aggregate = FALSE)

  ### Get unique concept ids
  conceptIds <- cov$covariateRef %>%
    dplyr::filter(analysisId == 210) %>%
    dplyr::select(conceptId) %>%
    dplyr::mutate(conceptId = as.double(conceptId)) %>%
    dplyr::collect() %>%
    dplyr::pull()

  conditionSql <-
    "with disease as ( -- define disease categories similar to ICD10 Chapters
    select 1 as precedence, 'Blood disease' as category_name, 440371 as category_id union
    select 1, 'Blood disease', 443723 union
    select 2, 'Injury and poisoning', 432795 union
    select 2, 'Injury and poisoning', 442562 union
    select 2, 'Injury and poisoning', 444363 union
    select 3, 'Congenital disease', 440508 union
    select 4, 'Pregnancy or childbirth disease', 435875 union
    select 4, 'Pregnancy or childbirth disease', 4088927 union
    select 4, 'Pregnancy or childbirth disease', 4154314 union
    select 4, 'Pregnancy or childbirth disease', 4136529 union
    select 5, 'Perinatal disease', 441406 union
    select 6, 'Infection', 432250 union
    select 7, 'Neoplasm', 438112 union
    select 8, 'Endocrine or metabolic disease', 31821 union
    select 8, 'Endocrine or metabolic disease', 4090739 union
    select 8, 'Endocrine or metabolic disease', 436670 union
    select 9, 'Mental disease', 432586 union
    select 10, 'Nerve disease and pain', 376337 union
    select 10, 'Nerve disease and pain', 4011630 union
    select 11, 'Eye disease', 4038502 union
    select 12, 'ENT disease', 4042836 union
    select 13, 'Cardiovascular disease', 134057 union
    select 14, 'Respiratory disease', 320136 union
    select 15, 'Digestive disease', 4302537 union
    select 16, 'Skin disease', 4028387 union
    select 17, 'Soft tissue or bone disease', 4244662 union
    select 17, 'Soft tissue or bone disease', 433595 union
    select 17, 'Soft tissue or bone disease', 4344497 union
    select 17, 'Soft tissue or bone disease', 40482430 union
    select 17, 'Soft tissue or bone disease', 4027384 union
    select 18, 'Genitourinary disease', 4041285 union
    select 19, 'Iatrogenic condition', 4105886 union
    select 19, 'Iatrogenic condition', 4053838
  )
  select distinct -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
    concept_id as condition_id, concept_name as condition_name,
    first_value(coalesce(category_id, 0)) over (partition by concept_id order by precedence nulls last) as category_id,
    first_value(coalesce(category_name, 'Other Condition')) over (partition by concept_id order by precedence nulls last) as category_name
  from @cdmDatabaseSchema.concept
  left join ( -- find the approprate disease category, if possible
    select descendant_concept_id, category_id, category_name, precedence
    from @cdmDatabaseSchema.concept_ancestor
    join disease on ancestor_concept_id=category_id
  ) d on descendant_concept_id=concept_id
  where concept_id in (@conceptIds) -- place here the concept_ids you want to roll up (have to be standard SNOMED)
  ;"

  cli::cat_bullet(
    "Rolling up conditions to ICD 10 Chapters",
    bullet = "tick",
    bullet_col = "green"
  )

  ## Split concept set into two vectors to avoid Snowflake error:
  ## maximum number of expressions in a list exceeded, expected at most 16,384, got 17,979

  chunk_length <- length(conceptIds)/2

  conceptIdList <- split(conceptIds,
                         ceiling(seq_along(conceptIds) / chunk_length))

  conceptIdsNo1 <- unlist(conceptIdList[1])
  conceptIdsNo2 <- unlist(conceptIdList[2])


  ## Get condition Rollup
  icd_chpNo1 <- DatabaseConnector::renderTranslateQuerySql(
    connection = con,
    sql = conditionSql,
    snakeCaseToCamelCase = TRUE,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conceptIds = conceptIdsNo1
  )

  icd_chpNo2 <- DatabaseConnector::renderTranslateQuerySql(
    connection = con,
    sql = conditionSql,
    snakeCaseToCamelCase = TRUE,
    cdmDatabaseSchema = cdmDatabaseSchema,
    conceptIds = conceptIdsNo2
  )

  icd_chp <- rbind(icd_chpNo1, icd_chpNo2)


  cov$icd_chp <- icd_chp %>%
    dplyr::rename(conceptId = conditionId, conceptName = conditionName, categoryCode = categoryId) %>%
    dplyr::mutate(
      categoryId = dplyr::case_when(
        categoryName == "Other Condition" ~ 0,
        categoryName == "Blood disease" ~ 1,
        categoryName == "Injury and poisoning" ~ 2,
        categoryName == "Congenital disease" ~ 3,
        categoryName == "Pregnancy or childbirth disease" ~ 4,
        categoryName == "Perinatal disease" ~ 5,
        categoryName == "Infection" ~ 6,
        categoryName == "Neoplasm" ~ 7,
        categoryName == "Endocrine or metabolic disease" ~ 8,
        categoryName == "Mental disease" ~ 9,
        categoryName == "Nerve disease and pain" ~ 10,
        categoryName == "Eye disease" ~ 11,
        categoryName == "ENT disease" ~ 12,
        categoryName == "Cardiovascular disease" ~ 13,
        categoryName == "Respiratory disease" ~ 14,
        categoryName == "Digestive disease" ~ 15,
        categoryName == "Skin disease" ~ 16,
        categoryName == "Soft tissue or bone disease" ~ 17,
        categoryName == "Genitourinary disease" ~ 18,
        categoryName == "Iatrogenic condition" ~ 19
      )
    ) %>%
    dplyr::select(categoryId, categoryCode, categoryName, conceptId, conceptName) %>%
    dplyr::arrange(categoryId, conceptId)

  cli::cat_bullet("Aggregating Covariates to ICD 10 Chapters",
                  bullet = "tick",
                  bullet_col = "green")

  condTbl2 <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::select(rowId, covariateValue, conceptId) %>%
    dplyr::left_join(cov$icd_chp, by = c("conceptId")) %>%
    dplyr::distinct(rowId, categoryId, categoryCode, categoryName) %>%
    dplyr::group_by(categoryId, categoryCode, categoryName) %>%
    dplyr::count(name = "countValue") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cohortId = cohortId) %>%
    dplyr::select(cohortId, categoryId, categoryCode, categoryName, countValue) %>%
    dplyr::arrange(categoryId) %>%
    dplyr::collect()

  verboseSave(object = condTbl2,
              savename = "conditions_baseline",
              saveLocation = outputFolder)
  invisible(condTbl2)

}


# Cohort Covariates -----------
cohortCovariates <- function(con,
                             cohortDatabaseSchema,
                             cohortTable,
                             cohortKey,
                             covariateKey,
                             timeA = -365,
                             timeB = -1,
                             outputFolder) {

  cli::cat_rule("Build Cohort Covariates")

  targetId <- cohortKey$id
  eventId <- covariateKey$id
  #sql to get cohort covariates
  sql <- "
    SELECT
      t.cohort_definition_id AS target_cohort_id,
      e.cohort_definition_id AS covariate_cohort_id,
      count(e.subject_id) as n
    FROM (
      SELECT *
      FROM @cohortDatabaseSchema.@cohortTable
      WHERE cohort_definition_id IN (@targetId)
    ) t
    JOIN (
      SELECT *
      FROM @cohortDatabaseSchema.@cohortTable
      WHERE cohort_definition_id IN (@eventId)
    ) e
    ON t.subject_id = e.subject_id
    AND e.cohort_start_date BETWEEN
          DATEADD(day, @timeA, t.cohort_start_date) AND
          DATEADD(day, @timeB, t.cohort_start_date)
    GROUP BY t.cohort_definition_id, e.cohort_definition_id
    ;
  "
  # Render and translate sql
  cohortCovariateSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    targetId = targetId,
    eventId = eventId,
    timeA = timeA,
    timeB = timeB
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  #run query on connection
  cohortCovTbl <- DatabaseConnector::querySql(con, sql = cohortCovariateSql) %>%
    rename(
      cohortId = TARGET_COHORT_ID,
      covariateId = COVARIATE_COHORT_ID,
      count = N
    ) %>%
    dplyr::left_join(
      cohortKey, by = c("cohortId" = "id")
    ) %>%
    dplyr::rename(cohortName = name) %>%
    dplyr::left_join(
      covariateKey, by = c("covariateId" = "id")
    ) %>%
    dplyr::rename(covariateName = name) %>%
    dplyr::mutate(
      pct = count / n
    ) %>%
    dplyr::select(
      cohortId, cohortName, covariateId, covariateName, count, pct
    )

  fname <- paste("cohort_covariates", abs(timeA), abs(timeB), sep = "_")
  save_path <- fs::path(outputFolder, fname , ext = "csv")
  readr::write_csv(cohortCovTbl, file = save_path)
  cli::cat_bullet("Saved to:\n", crayon::cyan(save_path), bullet = "info", bullet_col = "blue")
  invisible(save_path)


}


# Execute Module ----------------------

executeBaselineCharacteristicsModule <- function(executionSettings,
                                                 con,
                                                 cohortKey,
                                                 covariateKey,
                                                 timeA = -365,
                                                 timeB = -1,
                                                 outputFolder) {
  cohortId <- cohortKey$id
  cli::cat_boxx("Building Baseline Covariates")
  cli::cat_bullet("Using Baseline Window: [", crayon::green(timeA), ", ", crayon::green(timeB), "]", bullet = "info", bullet_col = "blue")
  cat_cohortId <- paste(cohortId, collapse = ", ")
  cli::cat_bullet("Building baseline covariates for cohort ids:\n   ", crayon::green(cat_cohortId),
                  bullet = "info", bullet_col = "blue")
  cli::cat_line()

  cohortDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema

  # RUn baseline covariates
  tik <- Sys.time()

  # Step 1: Run Baseline Demographics
  baselineDemographics(con = con,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortTable = cohortTable,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortId = cohortId,
                       outputFolder = outputFolder)

  # Step 2: Run Baseline Continuous
  baselineContinuous(con = con,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortId = cohortId,
                     timeA = -365,
                     timeB = -1,
                     outputFolder = outputFolder)

  # Step 3: Run Baseline Drug
  baselineDrugs(con = con,
                cdmDatabaseSchema = cdmDatabaseSchema,
                cohortTable = cohortTable,
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortId = cohortId,
                timeA = -365,
                timeB = -1,
                outputFolder = outputFolder)

  # Step 4: Run Baseline Conditions
  baselineConditions(con = con,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortId = cohortId,
                     timeA = -365,
                     timeB = -1,
                     outputFolder = outputFolder)

  # Step 5: Run Baseline Procedures
  baselineProcedures(con = con,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortId = cohortId,
                     timeA = -365,
                     timeB = -1,
                     outputFolder = outputFolder)

  # Step 6: Run Baseline Cohorts
  cohortCovariates(con = con,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTable = cohortTable,
                   cohortKey = cohortKey,
                   covariateKey = covariateKey,
                   timeA = -365,
                   timeB = -1,
                   outputFolder = outputFolder)

  # Step 7: Run Condition Groups
  buildAndBindConditionGroups(con = con,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              cohortTable = cohortTable,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortKey = cohortKey,
                              timeA = -365,
                              timeB = -1,
                              outputFolder = outputFolder)

  tok <- Sys.time()
  cli::cat_bullet("Execution Completed at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")
  invisible(tok)

}

