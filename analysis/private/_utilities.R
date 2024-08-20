# A. File Info -----------------------

# Task: Execution Settings
# Description: The purpose of the _executionSettings.R script is to set the execution settings and initialize cohorts


# B. Functions ------------------------

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


bindFiles <- function(inputPath,
                      pattern = NULL)  {

  ## Check if <pattern>.csv file exists. If it does, delete
  if (file.exists(here::here(inputPath, paste0(pattern, ".csv"))) == TRUE) {
        unlink(here::here(inputPath, paste0(pattern, ".csv")))
  }

  ## List all files with "pattern" in folder
  filepath <- list.files(inputPath, full.names = TRUE, pattern = pattern, recursive = TRUE)

  ## Read all the files and save in list
  listed_files <- lapply(filepath, readr::read_csv, show_col_types = FALSE)

  ## Bind all data frames of list together
  binded_df <- dplyr::bind_rows(listed_files)

  ## Save output
  readr::write_csv(
    x = binded_df,
    file = file.path(here::here(inputPath, paste0(pattern, ".csv"))),
    append = FALSE
  )

  ## Delete binded csv files
  unlink(filepath)

}


zipResults <- function(database) {

  resultsPath <- here::here("results", database)

  # Zip "report" folder and exclude treatment history folders and rds files
  files2zip <- dir(resultsPath, full.names = TRUE, recursive = TRUE)
  files2zip <- files2zip[!grepl("treatmentHistory", files2zip)]               # Exclude treatment history folder
  files2zip <- files2zip[!grepl("10_timeToDiscontinuation2_rds", files2zip)]  # Exclude rds files
  files2zip <- files2zip[!grepl("10_timeToDiscontinuation_rds", files2zip)]   # Exclude rds files
  files2zip <- files2zip[!grepl("12_timeToIntervention_rds", files2zip)]      # Exclude rds files


  if (length(database) >1) {

    zipName <- 'reportFiles'
  }
  else {

    zipName <- paste0('reportFiles_', database)
  }

  utils::zip(zipfile = zipName, files = files2zip)

  cli::cat_bullet("Study results have been zipped and saved to:",
                  crayon::cyan(here::here(paste0(zipName, ".zip"))),bullet = "info", bullet_col = "blue")

}
