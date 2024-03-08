# A. File info ---------------

# The purpose of this file is to place the zipped results from the study into the Bayer s3 bucket


# B. Dependencies ---------------

library(aws.s3)
library(remotes)
library(Ulysses)
library(tidyverse, quietly = TRUE)


# C. Variables ---------------

s3Bucket <- "ehden-hmb-results" # the name of the s3 bucket to store results

keyringName <- "ehden_hmb" # the name of the keyring

keyringPassword <- "ohdsi" # password for keyring


# D. Set Keyring ---------------

### Set aws credentials....these will be emailed separately
Ulysses::setMultipleCredentials(creds = c("key", "secret"),
                                db = "aws",
                                keyringName = keyringName,
                                keyringPassword = keyringPassword)


# E. Get zip files from Cohort Diagnostics ---------------

siteName <- "test" # place your site name
zipPathName <- paste(siteName, "results", sep = "_")

### List all files to zip
filesToZipAll <-  fs::dir_ls(path = "results", recurse = TRUE, type = "file")
filesToZip <- filesToZipAll[!grepl("treatmentHistory", filesToZipAll)]

### Zip files into a results zip folder
utils::zip(zipPathName, files = filesToZip)

### Zip path location for new zip folder
zipFilePath <- fs::path_abs(zipPathName) %>% fs::path(ext = "zip")

### Put zip file in the aws bucket
aws.s3::put_object(
  file = zipFilePath,
  object = basename(zipFilePath),
  bucket = s3Bucket,
  multipart = TRUE,
  region = "eu-central-1",
  key = keyring::key_get("aws_key", keyring = keyringName),
  secret = keyring::key_get("aws_secret", keyring = keyringName)
)

