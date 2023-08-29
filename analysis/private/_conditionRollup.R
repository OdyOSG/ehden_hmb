# A. Meta Info -----------------------

# Title: ICD10Chapter Rollup
# Author: Artem Gorbachev
# Date: 2023-08-11
# Version: 0.0.1
# Description: These internal function that allow for the rollup to ICD10 chapters used in Odysseus table 1

# B. Sql -----------------

icd10AggSql <- "
DROP TABLE IF EXISTS @targetDatabaseSchema.@icdCodesTable;
CREATE TABLE @targetDatabaseSchema.@icdCodesTable AS
WITH disease AS ( -- define disease categories similar to ICD10 Chapters
                SELECT 1 AS precedence, 'Blood disease' AS category_name, 440371 AS category_id
                UNION
                SELECT 1, 'Blood disease', 443723
                UNION
                SELECT 2, 'Injury and poisoning', 432795
                UNION
                SELECT 2, 'Injury and poisoning', 442562
                UNION
                SELECT 2, 'Injury and poisoning', 444363
                UNION
                SELECT 3, 'Congenital disease', 440508
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 435875
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 4088927
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 4154314
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 4136529
                UNION
                SELECT 5, 'Perinatal disease', 441406
                UNION
                SELECT 6, 'Infection', 432250
                UNION
                SELECT 7, 'Neoplasm', 438112
                UNION
                SELECT 8, 'Endocrine or metabolic disease', 31821
                UNION
                SELECT 8, 'Endocrine or metabolic disease', 4090739
                UNION
                SELECT 8, 'Endocrine or metabolic disease', 436670
                UNION
                SELECT 9, 'Mental disease', 432586
                UNION
                SELECT 10, 'Nerve disease and pain', 376337
                UNION
                SELECT 10, 'Nerve disease and pain', 4011630
                UNION
                SELECT 11, 'Eye disease', 4038502
                UNION
                SELECT 12, 'ENT disease', 4042836
                UNION
                SELECT 13, 'Cardiovascular disease', 134057
                UNION
                SELECT 14, 'Respiratory disease', 320136
                UNION
                SELECT 15, 'Digestive disease', 4302537
                UNION
                SELECT 16, 'Skin disease', 4028387
                UNION
                SELECT 17, 'Soft tissue or bone disease', 4244662
                UNION
                SELECT 17, 'Soft tissue or bone disease', 433595
                UNION
                SELECT 17, 'Soft tissue or bone disease', 4344497
                UNION
                SELECT 17, 'Soft tissue or bone disease', 40482430
                UNION
                SELECT 17, 'Soft tissue or bone disease', 4027384
                UNION
                SELECT 18, 'Genitourinary disease', 4041285
                UNION
                SELECT 19, 'Iatrogenic condition', 4105886
                UNION
                SELECT 19, 'Iatrogenic condition', 4053838
                )
SELECT DISTINCT -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
                c.concept_id AS concept_id,
                concept_name AS concept_name,
                first_value(coalesce(category_id, 0))
                OVER (PARTITION BY c.concept_id ORDER BY precedence, category_id NULLS LAST) AS category_code,
                first_value(coalesce(category_name, 'Other Condition'))
                OVER (PARTITION BY c.concept_id ORDER BY precedence, category_id NULLS LAST) AS category_name,
                first_value(coalesce(precedence, 0))
                OVER (PARTITION BY c.concept_id ORDER BY precedence, category_id NULLS LAST) AS category_id
FROM @cdmDatabaseSchema.concept c
JOIN (SELECT DISTINCT CAST(covariate_id / 1000 AS INT) AS concept_id FROM @targetDatabaseSchema.covariates) cs
    ON c.concept_id = cs.concept_id
LEFT JOIN ( -- find the approprate disease category, if possible
          SELECT descendant_concept_id, category_id, category_name, precedence
          FROM @cdmDatabaseSchema.concept_ancestor
          JOIN disease
              ON ancestor_concept_id = category_id
          ) D
    ON descendant_concept_id = c.concept_id;
"

getIcd10Sql <- "
SELECT cohort_id, category_id, category_code, category_name, count(*) AS countValue
FROM (
  SELECT DISTINCT @cohortId AS cohort_id, row_id, category_id, category_code, category_name
  FROM @targetDatabaseSchema.covariates cs
  JOIN @targetDatabaseSchema.@icdCodesTable icd
      ON icd.concept_id = CAST(cs.covariate_id / 1000 AS INT)
      ) tab
GROUP BY cohort_id, category_id, category_id, category_name, category_code
ORDER BY category_id;
"



# C. R Funtions ------------------------
getIcd10Chapters <- function(con,
                             cohortDatabaseSchema,
                             cohortTable,
                             cdmDatabaseSchema,
                             cohortId,
                             timeA,
                             timeB) {


  cli::cat_bullet("Building ICD 10 Rollup for cohort id:\n   ", crayon::green(cohortId),
                  bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  #define tables in database
  targetCovariateTable <- "covariates"
  icdCodesTable <- "icd_codes"

  # define covariate settings for condition groups
  covSettings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # if targetCovariate table persists for some reason:
  dropTableSql <- 'DROP TABLE IF EXISTS @targetDatabaseSchema.@targetCovariateTable'
  cli::cat_bullet("Drop covariates table", bullet = "info", bullet_col = "blue")
  DatabaseConnector::renderTranslateExecuteSql(connection = con,
                                               sql = dropTableSql,
                                               targetDatabaseSchema = cohortDatabaseSchema,
                                               targetCovariateTable = targetCovariateTable)
  cli::cat_line()

  # run feature extraction and place results in covariates table
  cli::cat_bullet("1) Build temp covariates table in database", bullet = "info", bullet_col = "blue")
  FeatureExtraction::getDbDefaultCovariateData(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = paste(cohortDatabaseSchema, cohortTable, sep = "."),
    cohortId = cohortId,
    covariateSettings = covSettings,
    targetDatabaseSchema = cohortDatabaseSchema,
    targetCovariateTable = targetCovariateTable,
    aggregated = FALSE
  )
  cli::cat_line()
  # generate ICD Chapters features
  cli::cat_bullet("2) Rollup ICD Chapters features", bullet = "info", bullet_col = "blue")
  DatabaseConnector::renderTranslateExecuteSql(
    connection = con,
    sql = icd10AggSql,
    targetDatabaseSchema = cohortDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    icdCodesTable = icdCodesTable
  )
  cli::cat_line()
  # retrieve ICD features
  cli::cat_bullet("3) Retrieve ICD Chapters features", bullet = "info", bullet_col = "blue")
  icdCovTab <- DatabaseConnector::renderTranslateQuerySql(
    connection = con,
    sql = getIcd10Sql,
    targetDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    icdCodesTable = icdCodesTable
  )
  cli::cat_line()
  # remove temporary tables
  tabs <- c(targetCovariateTable, icdCodesTable)
  # function to write drop table sql
  dropTableSql <- function(t) {
    paste0('DROP TABLE IF EXISTS @targetDatabaseSchema.', t, ';\n')
  }

  # build multiple sql for table drop
  sql <- purrr::map_chr(tabs, ~dropTableSql(.x)) |>
    paste0(collapse = "")

  #execute table drop
  cli::cat_bullet("4) Clean up")
  DatabaseConnector::renderTranslateExecuteSql(
    connection = con,
    sql = sql,
    targetDatabaseSchema = cohortDatabaseSchema
  )
  cli::cat_line()
  return(icdCovTab)

  # # save results
  # fname <- paste("condition_chapters_", cohortId, sep = "_")
  # tmpDir <- fs::path(outputFolder, "tmp") %>%
  #   fs::dir_create()
  # save_path <- fs::path(tmpDir, fname , ext = "csv")
  # readr::write_csv(icdCovTab, file = save_path)


}

executeConditionRollup <- function(con,
                                   executionSettings,
                                   analysisSettings) {


  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  # create output folder
  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  #extract cohorts for rollup
  cohortKey <- analysisSettings$baselineCharacteristics$cohorts$targetCohort

  #extract time windows
  timeA <- analysisSettings$baselineCharacteristics$timeWindow$startDay[1]
  timeB <- analysisSettings$baselineCharacteristics$timeWindow$endDay[1]



  #get icd10 chapters
  icd10ChapDat <- purrr::map_dfr(cohortKey$id,
                                 ~getIcd10Chapters(con = con,
                                                   cohortDatabaseSchema = workDatabaseSchema,
                                                   cohortTable = cohortTable,
                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                   cohortId = .x,
                                                   timeA = timeA,
                                                   timeB = timeB)
  )

  # save results
  verboseSave(
    object = icd10ChapDat,
    saveName = "condition_chapters",
    saveLocation = outputFolder
  )

  invisible(icd10ChapDat)
}
