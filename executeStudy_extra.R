# A. File Info -----------------------

# Name: Execute Study - Extra
# Description: The purpose of executeStudyExtra.R is to run the extra task required for the study


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

### Extra task
runStudyTask(file = studyTaskFiles[9], configBlock = configBlock)

