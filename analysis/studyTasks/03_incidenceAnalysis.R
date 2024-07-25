# A. File Info -----------------------

# Name: Incidence Analysis
# Description: The purpose of this script is to run incidence analyses


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(IncidencePrevalence)
library(CDMConnector)
source("analysis/private/_utilities.R")
source("analysis/private/_incidenceAnalysis.R")


# C. Connection ----------------------

### Set connection Block
# <<<
configBlock <- "[block]"
# >>>

### Provide connection details
executionSettings <- config::get(config = configBlock)
dbName <- strsplit(executionSettings$cdmDatabaseSchema, split = ".", fixed = TRUE)[[1]][1]
schemaName <- strsplit(executionSettings$cdmDatabaseSchema, split = ".", fixed = TRUE)[[1]][2]
writeDbName <- strsplit(executionSettings$workDatabaseSchema, split = ".", fixed = TRUE)[[1]][1]
writeSchemaName <- strsplit(executionSettings$workDatabaseSchema, split = ".", fixed = TRUE)[[1]][2]

### Connect to database
con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = executionSettings$dbms,
  database = dbName,
  schema = schemaName,
  uid = executionSettings$user,
  role = executionSettings$role,
  pwd = executionSettings$password
)

### Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(con, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
DBI::dbExecute(con, "ALTER SESSION SET JDBC_QUERY_RESULT_FORMAT='JSON'")

### Connect to database

cdm <- cdm_from_con(
  con = con,
  cdm_schema = c(catalog = dbName, schema = schemaName),
  write_schema = c(catalog = writeDbName, schema = writeSchemaName),
  cdm_name = dbName,
  .soft_validation = FALSE
)


# D. Variables -----------------------

### Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("user", "password", "connectionString"))

### Load analysis settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/incidenceAnalysis.yml"))


# E. Script --------------------

### Incidence Analyses

executeIncidenceAnalysis(cdm = cdm,
                         executionSettings = executionSettings,
                         analysisSettings = analysisSettings)


# F. Session Info ------------------------

DBI::dbDisconnect(con)
