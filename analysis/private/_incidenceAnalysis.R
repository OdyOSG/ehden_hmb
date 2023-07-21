# A. Meta Info -----------------------

# Task: Incidence Analysis
# Author: Martin Lavallee
# Date: 2023-07-21
# Description: The purpose of the _incidenceAnalysis.R script is to
# provide functions for the incidence analysis

# B. Functions ------------------------

runIncidenceAnalysis <- function(
    con,
    executionSettings,
    analysisSettings
) {

  # Step 0: Setup --------------

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  ## get cohort Ids
  targetCohortId <- analysisSettings$incidenceAnalysis$cohorts$outcomes$id
  targetCohortName <- analysisSettings$incidenceAnalysis$cohorts$outcomes$name
  denomCohortId <- analysisSettings$incidenceAnalysis$cohorts$denominator$id
  denomCohortName <- analysisSettings$incidenceAnalysis$cohorts$denominator$name
  irSettings <- analysisSettings$incidenceAnalysis$incidenceSettings

  # Step 1: Define Incidence Analysis -----------

  t1 <- CohortIncidence::createCohortRef(
    id = denomCohortId,
    name = denomCohortName
  )

  o1 <- CohortIncidence::createOutcomeDef(
    id = 1,
    name = "Outcome 1, 30d Clean",
    cohortId = targetCohortId,
    cleanWindow = irSettings$cleanWindow
  )

  tar1 <- CohortIncidence::createTimeAtRiskDef(
    id = 1,
    startWith = irSettings$startWith,
    startOffset = irSettings$startOffset,
    endWith = irSettings$endsWith,
    endOffset = irSettings$endOffset
  )

  analysis1 <- CohortIncidence::createIncidenceAnalysis(
    targets = c(t1$id),
    outcomes = c(o1$id),
    tars = c(tar1$id)
  )

  strata1 <- CohortIncidence::createStrataSettings(byYear = TRUE)

  irDesign <- CohortIncidence::createIncidenceDesign(
    targetDefs = list(t1),
    outcomeDefs = list(o1),
    tars = list(tar1),
    analysisList = list(analysis1),
    strataSettings = list(strata1)
  )

  # Step 2: Generate Incidence ------------------
  buildOptions <- CohortIncidence::buildOptions(
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    sourceName = databaseId,
    resultsDatabaseSchema = workDatabaseSchema,
    vocabularySchema = cdmDatabaseSchema,
    useTempTables = FALSE,
    refId = 1)


  cli::cat_line()
  cli::cat_bullet("Executing Incidence Analysis Id: ", crayon::green(refId),
                  bullet = "checkbox_on", bullet_col = "green")

  cli::cat_line()

  cli::cat_bullet("Denominator Population", bullet = "pointer", bullet_col = "yellow")
  cli::cat_line("   ", crayon::yellow(cli::symbol$star), " Cohort Id: ", crayon::green(denomCohortId), "\n",
                "   ", crayon::yellow(cli::symbol$star)," Cohort Name: ", crayon::green(denomCohortName))

  cli::cat_bullet("Outcome Population", bullet = "pointer", bullet_col = "yellow")
  cli::cat_line("   ", crayon::yellow(cli::symbol$star), " Cohort Id: ", crayon::green(targetCohortId), "\n",
                "   ", crayon::yellow(cli::symbol$star)," Cohort Name: ", crayon::green(targetCohortName))

  cli::cat_bullet("Time at Risk", bullet = "pointer", bullet_col = "yellow")
  cli::cat_line("   ", crayon::yellow(cli::symbol$star), " starts from " , crayon::green(irSettings$startOffset),
                " days from ", irSettings$startWith,"\n",
                "   ", crayon::yellow(cli::symbol$star), " ends from " , crayon::green(irSettings$endOffset),
                " days from ", irSettings$endsWith)

  executeResults <- CohortIncidence::executeAnalysis(
    connection = con,
    incidenceDesign = incidenceDesign,
    buildOptions = buildOptions)

  return(executeResults)

}
