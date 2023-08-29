# A. Meta Info -----------------------

# Task: Build Strata
# Author: Martin Lavallee
# Date: 2023-07-26
# Description: The purpose of the _buildStrata.R script is to build strata for the analysis

# B. Functions ------------------------

## Strata Funcitons-------------


ageStrata <- function(con,
                      cohortDatabaseSchema,
                      cohortTable,
                      cdmDatabaseSchema,
                      targetId,
                      strataId,
                      ageMin,
                      ageMax) {

  cli::cat_bullet("Building age strata between ", ageMin, "-" ,ageMax,
                  bullet = "checkbox_on", bullet_col = "green")

  sql <- "
    SELECT
    t2.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
    t2.subject_id,
    t2.cohort_start_date,
    t2.cohort_end_date
  INTO #age
  FROM (
    SELECT
      t1.cohort_definition_id,
      t1.subject_id,
      t1.cohort_start_date,
      t1.cohort_end_date,
      CASE
        WHEN age between @ageMin and @ageMax THEN 1
        ELSE 0
      END AS ageStrata
    FROM (
      SELECT c.cohort_definition_id,
             c.subject_id,
             c.cohort_start_date,
             c.cohort_end_date,
             p.year_of_birth,
             abs(p.year_of_birth - EXTRACT(YEAR FROM c.cohort_start_date)) AS age
      FROM @cohortDatabaseSchema.@cohortTable c
      JOIN @cdmDatabaseSchema.person p
        ON p.person_id = c.subject_id
      WHERE c.cohort_definition_id IN (@targetId)
      ) t1
    ) t2
  WHERE t2.ageStrata = 1;

  DELETE FROM @cohortDatabaseSchema.@cohortTable
  WHERE cohort_definition_id in (select cohort_definition_id from #age);

  INSERT INTO @cohortDatabaseSchema.@cohortTable (
        	cohort_definition_id,
        	subject_id,
        	cohort_start_date,
        	cohort_end_date
  )
  select * from #age;

  DROP TABLE #age;
"

  ageStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    targetId = targetId,
    strataId = strataId,
    ageMin = ageMin,
    ageMax = ageMax) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, ageStrataSql, progressBar = FALSE)

  #TODO Add timing
  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")
  cli::cat_bullet("Age strata written to ", cohortSchemaTable,
                  "using ids: ", paste(cohortStrataId, collapse = ", "),
                  bullet = "tick", bullet_col = "green")

  invisible(ageStrataSql)
}




buildStrata <- function(con,
                        executionSettings,
                        analysisSettings) {

  # Step 0: Prep

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings$strata$outputFolder) %>%
    fs::dir_create()


  ## get cohort Ids
  targetCohorts <- analysisSettings$strata$cohorts$targetCohort
  cohortStrata <- analysisSettings$strata$cohorts$strataCohorts %>%
    dplyr::rename(
      strataId = id,
      strataName = name
    )
  demoStrata <- analysisSettings$strata$demographics

  cli::cat_rule("Building Demographic Strata")

  tb1 <- expand_grid(targetCohorts, demoStrata) %>%
    dplyr::mutate(
      strataId = id * 1000 + strataId,
      strataName = paste(name, strataName)
    ) %>%
    select(strataId, strataName)

  # age strata 1 women under 30
  ageStrata(con,
            cohortDatabaseSchema = workDatabaseSchema,
            cohortTable = cohortTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            targetId = targetCohorts$id,
            strataId = demoStrata$strataId[1],
            ageMin = 9,
            ageMax = 29)

  # age strata 2 women 30-44
  ageStrata(con,
            cohortDatabaseSchema = workDatabaseSchema,
            cohortTable = cohortTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            targetId = targetCohorts$id,
            strataId = demoStrata$strataId[2],
            ageMin = 30,
            ageMax = 44)

  # age strata 3 women 45-55
  ageStrata(con,
            cohortDatabaseSchema = workDatabaseSchema,
            cohortTable = cohortTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            targetId = targetCohorts$id,
            strataId = demoStrata$strataId[3],
            ageMin = 45,
            ageMax = 56)


  strataKey <- tb1


  strataSummary <- dplyr::tbl(con, dbplyr::in_schema(workDatabaseSchema, cohortTable)) %>%
    dplyr::count(cohort_definition_id) %>%
    dplyr::collect() %>%
    dplyr::filter(cohort_definition_id > 1000)


  dt <- strataKey %>%
    dplyr::left_join(strataSummary, by = c("strataId" = "cohort_definition_id")) %>%
    dplyr::rename(cohort_definition_id = strataId,
                  name = strataName) %>%
    dplyr::select(cohort_definition_id, name, n)

  verboseSave(
    object = dt,
    saveName = "strata_table",
    saveLocation = outputFolder
  )

  invisible(dt)


}

