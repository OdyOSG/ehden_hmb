# A. Meta Info -----------------------

# Task: Execution Settings
# Author: Martin Lavallee
# Date: 2023-04-12
# Description: The purpose of the _executionSettings.R script is to
# set the execution settings and initialize cohorts

# B. Functions ------------------------
#

getCohortManifest <- function(inputPath = here::here("cohortsToCreate")) {

  #get cohort file paths
  cohortFiles <- fs::dir_ls(inputPath, recurse = TRUE, type = "file", glob = "*.json")
  #get cohort names
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()
  #get cohort type
  cohortType <- fs::path_dir(cohortFiles) %>%
    basename() %>%
    gsub(".*_", "", .)

  #future addition of hash
  hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
    purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
    unname()

  #return tibble with info
  tb <- tibble::tibble(
    name = cohortNames,
    type = cohortType,
    hash = hash,
    file = cohortFiles %>% as.character()
  ) %>%
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )
  return(tb)
}


startSnowflakeSession <- function(con, executionSettings) {
  sql <- "
  ALTER SESSION SET JDBC_QUERY_RESULT_FORMAT='JSON';
    USE ROLE @user_role;
    USE SECONDARY ROLES ALL;
    USE DATABASE @write_database;
    USE SCHEMA @write_schema;
  "
  crd <- stringr::str_split_1(string = executionSettings$workDatabaseSchema, pattern = "\\.")

  sessionSql <- SqlRender::render(
    sql = sql,
    user_role = executionSettings$role,
    write_database = crd[1],
    write_schema = crd[2]
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, sql = sessionSql)
  cli::cat_line("Setting up Snowflake session")

  invisible(sessionSql)
}


readSettingsFile <- function(settingsFile) {

  tt <- yaml::read_yaml(file = settingsFile)

  # convert cohorts into dataframes
  for (i in seq_along(tt[[1]][[1]])) {
    tt[[1]][[1]][[i]] <- listToTibble(tt[[1]][[1]][[i]])
  }

  #convert unnamed lists into dataframes
  ss <- seq_along(tt[[1]])
  for (j in ss[-1]) {
    check <- is.list(tt[[1]][[j]]) && is.null(names(tt[[1]][[j]]))
    if (check) {
      tt[[1]][[j]] <- listToTibble(tt[[1]][[j]])
    } else {
      next
    }
  }

  return(tt)
}

listToTibble <- function(ll) {
  df <- do.call(rbind.data.frame, ll) |>
    tibble::as_tibble()
  return(df)
}

