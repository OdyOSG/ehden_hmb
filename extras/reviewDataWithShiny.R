
### Step 1: Run migration script to populate 'shiny/data' folder with files
source(here::here("extras", "dataMigration.R"))


### Step 2: Once migration script is ran successfully, run the app in a browser
shiny::runApp(appDir = here::here("shiny"))

