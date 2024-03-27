# A. File Info -----------------------

# Name: Execute Study
# Description: The purpose of executeStudy.R is to run all the tasks required for the study


# B. Dependencies -----------------------

### Load functions to run study
source(here::here("analysis/private/_executeStudy.R"))


# C. Variables -----------------------

### Edit to respective config block
configBlock <- "optum"

### Provide path to tasks
studyTaskFolder <- here::here("analysis/studyTasks")
studyTaskFiles <- fs::dir_ls(studyTaskFolder, type = "file")


# D. Execute -----------------------

### Task 1: Build Cohorts
runStudyTask(file = studyTaskFiles[1], configBlock = configBlock)

### Task 2: Run Cohort Diagnostics
runStudyTask(file = studyTaskFiles[2], configBlock = configBlock)

### Task 3: Run Incidence Analysis
runStudyTask(file = studyTaskFiles[3], configBlock = configBlock)

### Task 4: Run Baseline Characteristics
runStudyTask(file = studyTaskFiles[4], configBlock = configBlock)

### Task 5: Run Treatment Patterns Analysis
runStudyTask(file = studyTaskFiles[5], configBlock = configBlock)

### Task 6: Run Procedure Analysis
runStudyTask(file = studyTaskFiles[6], configBlock = configBlock)

