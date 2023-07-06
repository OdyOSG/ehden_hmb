# File to store results of the study

# A) File info

# The purpose of this file is to place the zipped results from the study into the Bayer s3 bucket
# Author: Martin Lavallee


# B) Dependencies ---------------

#install.packages("aws.s3")
library(aws.s3)
#install.packages("remotes")
library(remotes)
# remotes::install_github("ohdsi/Ulysses")
library(Ulysses)
library(tidyverse, quietly = TRUE)

# c) Variables ----------------

s3Bucket <- "ehden-hmb-results" # the name of the s3 bucket to store results

keyringName <- "ehden_hmb" # the name of the keyring

keyringPassword <- "ulysses" # password for keyring

# D) Set Keyring  ----------------------
Ulysses::setMultipleCredentials(creds = c("key", "secret"),
                                db = "aws",
                                keyringName = keyringName,
                                keyringPassword = keyringPassword)

# Ulysses::checkDatabaseCredential(cred = "key", db = "aws", keyringName = "ehden_hmb")
# Ulysses::checkDatabaseCredential(cred = "secret", db = "aws", keyringName = "ehden_hmb")
# E) Get Zip files from Cohort Diagnostics -----------

resultsPath <- here::here("results")
db <- "synpuf_110k"
folder <- "02_cohortDiagnostics"

zipFilePath <- fs::path(resultsPath, db, folder) %>%
  fs::dir_ls(glob = "*.zip")

objZip <- basename(zipFilePath)

#put zip file in the aws bucket
aws.s3::put_object(
  file = zipFilePath,
  object = objZip,
  bucket = s3Bucket,
  key = keyring::key_get("aws_key", keyring = keyringName),
  secret = keyring::key_get("aws_secret", keyring = keyringName)
)

# do the same thing for the other databases.

# F. Session Info ------------------------

sessioninfo::session_info()
withr::deferred_run()
rm(list=ls())
