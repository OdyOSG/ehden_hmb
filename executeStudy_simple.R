# A. File Info -----------------------

# Name: Execute Study
# Description: The purpose of executeStudy.R is to run all the tasks required for the study


# B. Dependencies -----------------------

### Load functions to run study
source(here::here("analysis/private/_executeStudy.R"))


# C. Variables -----------------------

### Edit to respective config block
configBlock <- "[block]"

### Provide path to tasks
studyTaskFolder <- here::here("analysis/studyTasks")
studyTaskFiles <- fs::dir_ls(studyTaskFolder, type = "file")


# D. Execute -----------------------

### Task 3: Run Incidence Analysis
runStudyTask(file = studyTaskFiles[3], configBlock = configBlock)


