# File to share results of the study with coordinator

# A) File info

# The purpose of this file is to place the zipped results from the study into the Bayer s3 bucket
# Author: Martin Lavallee
# Date: 09/19/2023

# B) Dependencies ---------------

#install.packages("aws.s3")
library(aws.s3)
#install.packages("remotes")
library(remotes)
# remotes::install_github("ohdsi/Ulysses", ref = "develop)
library(Ulysses)
library(tidyverse, quietly = TRUE)

# c) Variables ----------------

s3Bucket <- "ehden-hmb-results" # the name of the s3 bucket to store results

keyringName <- "ehden_hmb" # the name of the keyring

keyringPassword <- "ohdsi" # password for keyring

# D) Set Keyring  ----------------------

# set aws credentials....these will be emailed separately
Ulysses::setMultipleCredentials(creds = c("key", "secret"),
                                db = "aws",
                                keyringName = keyringName,
                                keyringPassword = keyringPassword)


# E) Get Zip files from Cohort Diagnostics -----------
siteName <- "test" #place your site name
zipPathName <- paste(siteName, "results", sep = "_")

# list all files to zip
filesToZip <-  fs::dir_ls(path = "results", recurse = TRUE, type = "file")

#zip files into a results zip folder
utils::zip(zipPathName, files = filesToZip)

#zip path location for new zip folder
zipFilePath <- fs::path_abs(zipPathName) %>% fs::path(ext = "zip")

#put zip file in the aws bucket
aws.s3::put_object(
  file = zipFilePath,
  object = zipPathName,
  bucket = s3Bucket,
  multipart = TRUE,
  region = "eu-central-1",
  key = keyring::key_get("aws_key", keyring = keyringName),
  secret = keyring::key_get("aws_secret", keyring = keyringName)
)


