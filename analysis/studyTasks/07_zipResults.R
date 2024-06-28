# A. File Info ----------------------

# Name: Zip Results
# Description: The purpose of this script is to zip result files


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_utilities.R")


## Set connection block
# <<<
configBlock <- "[block]"
# >>>


# C. Script ----------------------

zipResults(database = configBlock)
