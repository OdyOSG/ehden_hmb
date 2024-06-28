# Preview results -------------
# The purpose of this file is to launch a local shiny app to review the results of the analysis conducted for ehden_hmb
# Once the study has been executed results will be saved to a folder separated by database name.


### Step 1: Run migration script to populate 'shiny/data' folder with files
source(here::here("shiny/migration", "dataMigration.R"))


### Step 2: Once migration script is ran successfully, run the app in a browser
shiny::runApp(appDir = here::here("shiny"))
