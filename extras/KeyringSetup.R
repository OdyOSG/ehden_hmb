# Setup Credentials -------------
# This file setups the credential library for your study.
# The function establishes a config.yml file and creates a keyring for the study.
# Input your credentials into the keyring. Keep your database credentials handy before running this script.
# Ask your database administrator if you are unsure of your credentials.


## A. Dependencies ------------

library(tidyverse, quietly = TRUE)
library(Ulysses)
library(keyring)


## B. Set Parameters ------------

configBlock <- "" # Name of config block

database <- ""    # Name of the database in the config block

keyringName <- "ehden_hmb"       # Name of the keyring

keyringPassword <- "ohdsi"       # Password for keyring


## C. Check or create Config File ------------

### Check if config.yml file exists; create it if it does not exist
checkConfig()


## D. Setup Keyring ------------

### Set keyring
setStudyKeyring(keyringName = keyringName,
                keyringPassword = keyringPassword)

### Set credential keys in keyring
setMultipleCredentials(cred = defaultCredentials(),
                       db = configBlock,
                       keyringName = keyringName,
                       keyringPassword = keyringPassword,
                       forceCheck = TRUE
)

### If a single credential is incorrect, change it
# setCredential(cred = "dbms",
#                       db = configBlock,
#                       keyringName = keyringName,
#                       keyringPassword = keyringPassword,
#                       forceCheck = TRUE
# )


## E. Check Credentials (Optional) ------------

### Test connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

connectionDetails$dbms


# G. Close out ------------

sessioninfo::session_info()
rm(list=ls())
