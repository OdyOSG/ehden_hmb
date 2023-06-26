# Upload Results to Study Host

# A) Info ------------

# Author: Martin Lavallee
# Date: 06/26/2023
# Description:
# This script uploads the results of the ehden hmb study to a dropbox. To run this file you need
# to be sent a dropbox token for the ehden_hmb study.

# B) Dependencies --------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)

# C) Variables -----------------

databaseName <- "[databaseName]" #the databaseName to use for results sharing
resultsLocation <- here::here("results") # location of the results folder


# D) Script ----------------

# create path to results
resPath <- fs::path(resultsLocation, databaseName)

# create new folder for upload
zipPath <- fs::path(resultsLocation, "upload") %>%
  fs::dir_create() %>%
  fs::path(databaseName, ext = "zip")

# compress results as a zip File
DatabaseConnector::createZipFile(
  zipFile = zipPath,
  files = resPath,
  rootFolder = resPath
)

# IMPORTANT!!!!
# When done upload zip files to Teams Environment.


# E) Session Info ------------------------

sessioninfo::session_info()
rm(list=ls())
