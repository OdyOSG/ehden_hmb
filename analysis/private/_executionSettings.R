# A. Meta Info -----------------------

# Task: Execution Settings
# Author: Martin Lavallee
# Date: 2023-04-12
# Description: The purpose of the _executionSettings.R script is to
# set the execution settings and initialize cohorts

# B. Functions ------------------------
#
# getExecutionSettings <- function(configBlock, file = here::here("config.yml")) {
#
#   connectionDetails <- DatabaseConnector::createConnectionDetails(
#     dbms = config::get('dbms', config = configBlock, file = file),
#     user = config::get('user', config = configBlock, file = file),
#     password = config::get('password', config = configBlock, file = file),
#     connectionString = config::get('connectionString', config = configBlock, file = file)
#     )
#
#
#   executionSettings <- structure(list(
#     'connectionDetails' = connectionDetails,
#     'cdmDatabase' = config::get("cdmDatabase", config = configBlock, file = file),
#     'cdmSchema' = config::get("cdmSchema", config = configBlock, file = file),
#     'vocabularySchema' = config::get("vocabSchema", config = configBlock, file = file),
#     'writeDatabase' = config::get("writeDatabase", config = configBlock, file = file),
#     'writeSchema' = config::get("writeSchema", config = configBlock, file = file),
#     'cohortTable' = config::get("cohortTable", config = configBlock, file = file),
#     'databaseId' = config::get("databaseName", config = configBlock, file = file),
#     'userRole'= config::get("role", config = configBlock, file = file)
#   ), class = "executionSettings")
#
#
#   return(executionSettings)
# }


startSnowflakeSession <- function(con, executionSettings) {
  sql <- "
  ALTER SESSION SET JDBC_QUERY_RESULT_FORMAT='JSON';
    USE ROLE @user_role;
    USE SECONDARY ROLES ALL;
    USE DATABASE @write_database;
    USE SCHEMA @write_schema;
  "
  sessionSql <- SqlRender::render(
    sql = sql,
    user_role = executionSettings$userRole,
    write_database = executionSettings$writeDatabase,
    write_schema = executionSettings$writeSchema
  ) %>%
    SqlRender::translate(targetDialect = executionSettings$connectionDetails$dbms)
  DatabaseConnector::executeSql(connection = con, sql = sessionSql)
  cli::cat_line("Setting up Snowflake session")
  invisible(sessionSql)
}


initializeCohortTables <- function(executionSettings, con) {

  if (con@dbms == "snowflake") {
    workSchema <- paste(executionSettings$workDatabase, executionSettings$workSchema, sep = ".")
  } else {
    workSchema <- executionSettings$workSchema
  }


  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  CohortGenerator::createCohortTables(connection = con,
                                      cohortDatabaseSchema = workSchema,
                                      cohortTableNames = cohortTableNames,
                                      incremental = TRUE)
  invisible(cohortTableNames)

}

