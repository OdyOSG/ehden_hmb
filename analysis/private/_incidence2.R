# A. Meta Info -----------------------

# Task: Incidence 2
# Author: Martin Lavallee
# Date: 2023-08-24
# Description: The purpose of the _incidence2.R script is to.....

# B. Functions ------------------------

generateIncidence <- function(con,
                              executionSettings,
                              analysisSettings) {

  ## Prep
  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  # establish Cdm connector connection
  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = cdmDatabaseSchema,
                                    write_schema = workDatabaseSchema,
                                    cohort_tables = cohortTable)


  # create denominator cohort set
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm,
    strataTable = cohortTable,
    strataCohortId = 1 # denominator cohort id PARAMETERIZE ME
  )

  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = 1, # denominator cohort id PARAMETERIZE ME
    outcomeTable = cohortTable,
    outcomeCohortId = 2, # outcome cohort id parameterize me
    interval = "overall", # analysis option
    outcomeWashout = 180L, # analysis option
    repeatedEvents = TRUE
  )

  # add saving of table
  return(inc)


}
