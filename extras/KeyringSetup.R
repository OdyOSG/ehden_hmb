# Setup Credentials ------------
# This file setups the credential library for the study. The function establishes
# a config.yml file and creates a keyring for the study. To add your credentials
# into the keyring keep your database credentials handy before running this script.
# Ask your database administrator if you are unsure of your credentials.

## A) Dependencies ------------

library(tidyverse, quietly = TRUE)
library(Ulysses)
library(keyring)
source("analysis/private/_keyring.R")


## B) Set Parameters ------------

configBlock <- "[block]"           # Name of config block

database <- "[database]"           # Name of the database in the config block

keyringName <- getKeyringName()    # Name of keyring

keyringPassword <- "[password]"    # Password for keyring


## C) Set config.yml ------------

checkConfig() # Check if config.yml file exists


## D) Setup Keyring ------------

checkKeyring(keyringName = keyringName, keyringPassword = keyringPassword) # Create the keyring for the study


## E) Set Credentials ------------

creds <- c(
  "dbms",                # The database dialect. Run ?DatabaseConnector::createConnectionDetails and scroll down to the 'Arguments' section to find the correct variable name for the dialect
  "user",                # The user's username for the database
  "password",            # The user's password for the database
  "connectionString",    # The connection string to access the database
  "cdmDatabaseSchema",   # The database + schema (or just schema) hosting OMOP CDM data
  "vocabDatabaseSchema", # The database + schema (or just schema) hosting the vocabulary, usually same the as cdmDatabaseSchema
  "workDatabaseSchema"   # The database + schema (or just schema) hosting the work or scratch space where study results will be stored
)

setCreds(creds = creds, configBlock = configBlock, keyringName = keyringName)


## F) Check credentials ------------

checkCreds(creds = creds, configBlock = configBlock, keyringName = keyringName) # Review that credentials are correct

## If a single credential is incorrect, change it by running the following function:
#setSingleCred(cred = "dbms", keyring = keyringName, configBlock = configBlock)

testCreds(configBlock = configBlock) # Test access to keyring


## G) Session Info ------------

sessioninfo::session_info()
rm(list=ls())
