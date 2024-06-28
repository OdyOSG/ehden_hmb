# A. File Info -----------------------

# Title: Concept Prevalence
# Description: These internal function run prevalence of concepts using Feature Extraction


# B. Helpers -----------------

silentCovariates <- function(con, cdmDatabaseSchema, cohortTable, cohortDatabaseSchema, cohortId, covSettings) {

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
    aggregated = TRUE
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


verboseSave <- function(object, saveName, saveLocation) {

  savePath <- fs::path(saveLocation, saveName, ext = "csv")
  readr::write_csv(object, file = savePath)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(savePath)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(saveLocation), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  invisible(savePath)
}


# C. Domain FE -------------------------

baselineDemographics <- function(con,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 cdmDatabaseSchema,
                                 cohortId,
                                 outputFolder) {

  cli::cat_rule("Build Demographic Covariates")

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

  demoTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect()

  verboseSave(
    object = demoTbl,
    saveName = "demographics_baseline",
    saveLocation = outputFolder
  )

  invisible(demoTbl)
}


baselineContinuous <- function(con,
                               cohortDatabaseSchema,
                               cohortTable,
                               cdmDatabaseSchema,
                               cohortId,
                               outputFolder) {

  cli::cat_rule("Build Continuous Covariates")

  # Create Continuous settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime = TRUE,
    useDemographicsTimeInCohort = TRUE
  )

  #run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  ctsTbl <- cov$covariatesContinuous %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, covariateId, name, minValue, p10Value, p25Value, medianValue, p75Value, p90Value, maxValue) %>%
    dplyr::collect()

  verboseSave(
    object = ctsTbl,
    saveName = "continuous_baseline",
    saveLocation = outputFolder
  )

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
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_" ,abs(timeB))
    )

  verboseSave(
    object = drugTbl,
    saveName = paste("drugs_baseline", abs(timeA), abs(timeB), sep = "_"),
    saveLocation = outputFolder
  )

  invisible(drugTbl)
}


baselineConditions <- function(con,
                               cohortDatabaseSchema,
                               cohortTable,
                               cdmDatabaseSchema,
                               cohortId,
                               timeA = -365,
                               timeB = -1,
                               outputFolder) {

  cli::cat_rule("Build Condition Covariates")

  # Create Drugs settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE,
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

  condTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_" ,abs(timeB))
    )

  verboseSave(
    object = condTbl,
    saveName = paste("conditions_baseline", abs(timeA), abs(timeB), sep = "_"),
    saveLocation = outputFolder
  )

  invisible(condTbl)
}


baselineProcedures <- function(con,
                               cohortDatabaseSchema,
                               cohortTable,
                               cdmDatabaseSchema,
                               cohortId,
                               timeA = -365,
                               timeB = -1,
                               outputFolder) {

  cli::cat_rule("Build Procedure Covariates")

  # Create Drugs settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useProcedureOccurrenceLongTerm = TRUE,
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

  procTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_" ,abs(timeB))
    )

  verboseSave(
    object = procTbl,
    saveName = paste("procedures_baseline", abs(timeA), abs(timeB), sep = "_"),
    saveLocation = outputFolder
  )

  invisible(procTbl)
}


# D. Execute ----------------------

executeConceptCharacterization <- function(con,
                                           executionSettings,
                                           analysisSettings) {

  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  ## get cohort Ids
  cohortKey <- analysisSettings$baselineCharacteristics$cohorts$targetCohort
  covariateKey <- analysisSettings$baselineCharacteristics$cohorts$covariateCohorts

  timeA <- analysisSettings$baselineCharacteristics$timeWindow$startDay
  timeB <- analysisSettings$baselineCharacteristics$timeWindow$endDay

  cohortId <- cohortKey$id
  cli::cat_boxx("Building Baseline Covariates")
  cli::cat_bullet("Using Baseline Window: [", crayon::green(timeA), ", ", crayon::green(timeB), "]", bullet = "info", bullet_col = "blue")
  cat_cohortId <- paste(cohortId, collapse = ", ")
  cli::cat_bullet("Building baseline covariates for cohort ids:\n   ", crayon::green(cat_cohortId),
                  bullet = "info", bullet_col = "blue")
  cli::cat_line()

  # Run baseline covariates
  tik <- Sys.time()

  # Step 1: Run Baseline Demographics
  baselineDemographics(con = con,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       cohortTable = cohortTable,
                       cohortDatabaseSchema = workDatabaseSchema,
                       cohortId = cohortId,
                       outputFolder = outputFolder)

  # Step 2: Run Baseline Continuous
  baselineContinuous(con = con,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = workDatabaseSchema,
                     cohortId = cohortId,
                     outputFolder = outputFolder)


  ## Loop through different time window combos
  for (i in seq_along(timeA)) {

  # Step 3: Run Baseline Drug
  baselineDrugs(con = con,
                cdmDatabaseSchema = cdmDatabaseSchema,
                cohortTable = cohortTable,
                cohortDatabaseSchema = workDatabaseSchema,
                cohortId = cohortId,
                timeA = timeA[i],
                timeB = timeB[i],
                outputFolder = outputFolder)

  # Step 4: Run Baseline Conditions
  baselineConditions(con = con,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = workDatabaseSchema,
                     cohortId = cohortId,
                     timeA = timeA[i],
                     timeB = timeB[i],
                     outputFolder = outputFolder)

  # Step 5: Run Baseline Procedures
  baselineProcedures(con = con,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortTable = cohortTable,
                     cohortDatabaseSchema = workDatabaseSchema,
                     cohortId = cohortId,
                     timeA = timeA[i],
                     timeB = timeB[i],
                     outputFolder = outputFolder)
  }

  # Bind "drug_baseline" csv files together (deletes binded csvs)
  bindFiles(
    inputPath = outputFolder,
    pattern = "drugs_baseline"
  )

  # Bind "conditions_baseline" csv files together (deletes binded csvs)
  bindFiles(
    inputPath = outputFolder,
    pattern = "conditions_baseline"
  )

  # Bind "procedures_baseline" csv files together (deletes binded csvs)
  bindFiles(
    inputPath = outputFolder,
    pattern = "procedures_baseline"
  )


  tok <- Sys.time()
  cli::cat_bullet("Execution Completed at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")

  invisible(tok)
}
