# Setup Credentials -------------
# This file setups the credential library for your study. The function establishes
# a config.yml file and creates a keyring for the study. Input your credentials
# into the keyring. Keep your database credentials handy before running this script.
# Ask your database administrator if you are unsure of your credentials.

## A) Depedendencies ------------

library(tidyverse, quietly = TRUE)
library(Ulysses)
library(keyring)

## B) Set Parameters ------------

configBlock <- "odysseus" # name of config block

database <- "synpuf_110k" # the name of the database in the config block

keyringName <- "ehden_hmb" # the name of the keyring

keyringPassword <- "ulysses" # password for keyring
# This password is simply to avoid a prompt when creating the keyring

## c) Create Config File------------------------

# create the config.yml file
makeConfig(block = configBlock, database = database)

## D) Setup Keyring -----------------

# Create the keyring if it does not exist.
# If it exists, clear it out so we can re-load the keys
allKeyrings <- keyring::keyring_list()
if (keyringName %in% allKeyrings$keyring) {
  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  # Delete all keys from the keyring so we can delete it
  cli::cat_bullet("Delete existing keyring: ", keyringName,
                  bullet = "warning", bullet_col = "yellow")
  keys <- keyring::key_list(keyring = keyringName)
  if (nrow(keys) > 0) {
    for (i in 1:nrow(keys)) {
      keyring::key_delete(keys$service[i], keyring = keyringName)
    }
  }
  keyring::keyring_delete(keyring = keyringName)
}
# set a new keyring for study
keyring::keyring_create(keyring = keyringName, password = keyringPassword)

## E) Set Credentials -----------------------
creds <- c(
  "dbms", # the database dialect
  #"role", # the role id for your user in the db, ONLY FOR BAYER
  "user", # the user name for the db
  "password", # the password for the db
  "connectionString", # the connection string to access the db
  "cdmDatabaseSchema", # the database + schema (or just schema) hosting the cdm
  "vocabDatabaseSchema", # the database + schema (or just schema) hosting the vocabulary, usually same as cdm
  "workDatabaseSchema" # the database + schema (or just schema) hosting the work or scratch
)

cred_block <- paste(configBlock, creds, sep = "_")

# set a new keyring for study
# this will lead to a dialog box enter the credentials
purrr::walk2(cred_block, creds, ~keyring::key_set(service = .x, keyring = keyringName, prompt = .y))


## F) Check (Optional) -----------------------


### Review that the credentials are correct

blurCreds <- function(item, configBlock, keyringName) {
  credential <- paste(configBlock, item, sep = "_")
  cred <- keyring::key_get(service = credential, keyring = keyringName)
  txt <- glue::glue(item, ": ", crayon::blurred(cred))
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")
  invisible(credential)
}

purrr::walk(creds, ~blurCreds(item = .x, configBlock = configBlock, keyringName = keyringName))

# If a single credential is incorrect, change it
#keyring::key_set(service = "dbms", kerying = keyringName)


### Test connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("user", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)
connectionDetails$dbms


# G) Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
