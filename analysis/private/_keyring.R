`%notin%` <- Negate("%in%")

makeConfigUpdated <- function(block, database = block, projectPath = here::here(), open = TRUE) {

  projFile <- list.files(projectPath, pattern = ".Rproj", full.names = TRUE)
  projName <- basename(tools::file_path_sans_ext(here::here(projFile)))

  data <- rlang::list2(Project = projName, Cohort = paste(projName,
                                                          database, sep = "_"), Block = block, Database = database)
  usethis::use_template(template = "config.yml", data = data,
                        open = open, package = "Ulysses")
  usethis::use_git_ignore(ignores = "config.yml")
  invisible(data)
}


addConfigUpdated <- function (block, database = block, projectPath = here::here(), open = TRUE) {

  check <- config_check()
  if (check) {

    projFile <- list.files(projectPath, pattern = ".Rproj", full.names = TRUE)
    projName <- basename(tools::file_path_sans_ext(here::here(projFile)))

    cohortTable <- paste(projName, database, sep = "_")
    config_block_txt <- glue::glue("\n\n# {block} Credentials\n\n{block}:\n  databaseName: {database}\n  dbms: !expr keyring::key_get('{block}_dbms', keyring = '{projName}')\n  user: !expr keyring::key_get('{block}_user', keyring = '{projName}'),\n  password: !expr keyring::key_get('{block}_password', keyring = '{projName}')\n  connectionString: !expr keyring::key_get('{block}_connectionString', keyring = '{projName}')\n  cdmDatabaseSchema: !expr keyring::key_get('{block}_cdmDatabaseSchema', keyring = '{projName}')\n  vocabDatabaseSchema: !expr keyring::key_get('{block}_vocabDatabaseSchema', keyring = '{projName}')\n  workDatabaseSchema: !expr keyring::key_get('{block}_workDatabaseSchema', keyring = '{projName}')\n  cohortTable: {cohortTable}\n  ")
    save_as <- fs::path(projectPath, "config.yml")
    write_utf8(path = save_as, lines = config_block_txt,
               append = TRUE)
    if (open) {
      rstudioapi::navigateToFile(save_as)
    }
    cli::cat_bullet("Added block ", crayon::green(block),
                    " to config.yml", bullet = "info", bullet_col = "blue")
  }
  else {
    cli::cat_bullet("config.yml does not exist", bullet = "warning",
                    bullet_col = "yellow")
    txt <- glue::glue("`Ulysses::makeConfig(block = {block}, database = {database})` ")
    cli::cat_line("To create config.yml edit and run function:\n\n  ",
                  crayon::red(txt), "\n")
  }
  cli::cat_bullet("Restart R session to implement changes to config.yml",
                  bullet = "info", bullet_col = "blue")
  invisible(check)
}


checkKeyring <- function(keyringPassword, keyringName) {

  allKeyrings <- keyring::keyring_list()

  if (keyringName %in% allKeyrings$keyring) {
    if (keyring::keyring_is_locked(keyring = keyringName)) {

      keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)

    }

    # Delete all keys from keyring so we can delete it
    cli::cat_bullet("Delete existing keyring: ", keyringName, bullet = "info", bullet_col = "yellow")

    keys <- keyring::key_list(keyring = keyringName)

    if (nrow(keys) > 0) {
      for (i in 1:nrow(keys)) {

        keyring::key_delete(keys$service[i], keyring = keyringName)

      }
    }

    # Delete keyring
    keyring::keyring_delete(keyring = keyringName)

    # Create keyring
    cli::cat_bullet("Keyring created: ", keyringName, bullet = "info", bullet_col = "blue")
    keyring::keyring_create(keyring = keyringName, password = keyringPassword)

  } else {

    cli::cat_bullet("Keyring created: ", keyringName, bullet = "info", bullet_col = "blue")
    keyring::keyring_create(keyring = keyringName, password = keyringPassword)

  }

}


blurCreds <- function(item, configBlock, keyringName) {

  credential <- paste(configBlock, item, sep = "_")

  cred <- keyring::key_get(service = credential, keyring = keyringName)

  txt <- glue::glue(item, ": ", crayon::blurred(cred))
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")

  invisible(credential)
}


checkCreds <- function (creds, configBlock, keyringName) {

  allKeyrings <- keyring::keyring_list()

  if (keyringName %notin% allKeyrings$keyring)
    {
      cli::cat_bullet("Keyring doesn't exist",  bullet = "warning", bullet_col = "yellow")

    }

  else if (keyringName %in% allKeyrings$keyring & nrow(keyring::key_list(keyring = keyringName)) > 0)

    {
      purrr::walk(creds, ~blurCreds(item = .x, configBlock = configBlock, keyringName = keyringName))
    }

  else if (keyringName %in% allKeyrings$keyring & nrow(keyring::key_list(keyring = keyringName)) == 0)

    {
      cli::cat_bullet("Keyring doesn't have any keys",  bullet = "warning", bullet_col = "yellow")
    }
}


setCreds <- function(creds, configBlock, keyringName) {

  cred_block <- paste(configBlock, creds, sep = "_")

  # This will open up a dialog box to add credentials
  purrr::walk2(cred_block, creds, ~keyring::key_set(service = .x, keyring = keyringName, prompt = .y))

  cli::cat_bullet("Credentials for keyring ", keyringName, " have been created", bullet = "info", bullet_col = "blue")

}


testCreds <- function(configBlock) {

  # Test connection details
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = config::get("dbms", config = configBlock),
    user = config::get("user", config = configBlock),
    password = config::get("password", config = configBlock),
    connectionString = config::get("connectionString", config = configBlock)
  )

  return(connectionDetails$dbms)

}


getKeyringName <- function(projectPath = here::here()) {

  projFile <- list.files(projectPath, pattern = ".Rproj", full.names = TRUE)
  projName <- basename(tools::file_path_sans_ext(here::here(projFile)))

  return(projName)
}


setSingleCred <- function(cred, configBlock, keyringName) {

  cred_block <- paste(configBlock, cred, sep = "_")

  keyring::key_set(service = cred_block, keyring = keyringName, prompt = cred)

}
