# How to Run Ehden HMB

## Table of Contents

1)  Technical Requirements
2)  Setting up the Ehden HMB study locally
3)  Setup Credentials
4)  Executing Scripts

## Pre-Study Technical Requirements

To run this study you must setup the [HADES environment](https://ohdsi.github.io/Hades/rSetup.html).

-   [R](https://cloud.r-project.org/) installed, (version 4.1 or greater)
-   [R Studio](https://posit.co/download/rstudio-desktop/) installed
-   On Windows: [RTools](https://cran.r-project.org/bin/windows/Rtools/) installed
-   [Java](https://www.java.com/en/) installed

You also require your site data to be mapped to the [OMOP CDM](https://ohdsi.github.io/CommonDataModel/) and administered on one of the following supported database platforms:

-   Microsoft SQL Server
-   Microsoft Parallel Data Warehouse
-   Oracle
-   PostgreSQL
-   Google BigQuery
-   Amazon RedShift
-   Snowflake
-   Apache Hive
-   Apache Spark
-   Apache Impala

## 2) Setting up the Ehden Hmb study locally

### Get Dependencies

### Get Repo

1)  Go to the github url: <https://github.com/OdyOSG/ehden_hmb>.
2)  Select the green code button revealing a dropdown menu.
3)  Select *Download Zip*.
4)  Unzip the folder on your local computer that is easily accessible within R Studio.
5)  Open the unzipped folder and select `ehden_hmb.Rproj`.

### Setup `renv`

This project contains an `renv.lock` file which ensures that all collaborators are working in the same environement when executing the *ehden_hmb* study. For more details on `renv`, refer to the [package website](https://rstudio.github.io/renv/index.html).

To activate the `renv` file, run:

```         
renv::restore()
```

If there are any issues with the `renv` please contact [martin.lavallee\@odysseusinc.com](mailto:martin.lavallee@odysseusinc.com){.email} or [george.argyriou\@odysseusinc.com](mailto:george.argyriou@odysseusinc.com){.email}.

### Final steps

To run the *ehden_hmb* study the user must install the following packages:

``` r
install.packages('usethis')
install.packages('remotes')
remotes::install_github("ohdsi/Ulysses", ref = "develop")
```

## 3) Setup Credentials

Have your database credentials ready; suggested to save them in a accessible txt file. If you need to know your database credentials contact your database administrator. Once you have your credentials ready, open the `extras/KeyringSetup.R` file in the *ehden_hmb* project.

**1) Assign Variables**

Load the dependencies on L9-11. If they have not been installed please install them using `install.packages` for CRAN packages or using the installation directions for the `Ulysses` package provided above.

Create a configBlock name for the database you want to use first. For example if you are working on the Optum claims, on L15 assign the `configBlock` variable as *"optum"*. The configBlock name is a shorthand reference to the database. Try to make this name a single word or two words separated by quotes. Next provide a name to the database. On L17 assign the `database` variable to a character string that describes the database in use. This can be the same name as the `configBlock`.

There are two more variables to set in section B of the `KeyringSetup.R` file, `keyringName` and `keyringPassword`. These variables pretain to the `keyring` setup. The keyring is the group that stores the set of credentials that pertain to the study. Name the `keyringName` after the study, this parameter should already be pre-registered as *bayerChapter*. Next, set a password for the keyring. Everytime we want to access the *bayerChapter* keyring, we must input this password. This password is preset as *ulysses* for convenience, however the user may choose to alter this. **Keep this password handy when accessing the keyring for the study.**

**2) Create config.yml**

Once the variables have been setup, you can now setup the *config.yml* file. Line 27 of the `KeyringSetup.R` file asks you to check if the *config.yml* file already exists. If it does not already exist run the following:

``` r
Ulysses::makeConfig(block = configBlock, database = database)
```

where the `configBlock` and `database` variables are defined in section B. Running this function will open the *config.yml* file in Rstudio. For Bayer projects you must add an additional credential, the role. Please copy and paste the below and make sure that it is tabbed in line with the block. Replace the section *[block]* with the value in your `configBlock` variable. Add a new line to the yaml file. **Make sure this new line is not tabbed.**

```         
role: !expr keyring::key_get('[block]_role', keyring = 'bayerChapter')
```

If the *config.yml* already exists. Open it and review. Make sure the block contains the following structure, where *[block]* is the value in your `configBlock` variable:

```         
# Config block example

block:
  databaseName: synpuf_110k
  cohortTable: bayerChapter_synpuf_110k
  dbms: !expr keyring::key_get('block_dbms', keyring = 'bayerChapter')
  user: !expr keyring::key_get('block_user', keyring = 'bayerChapter')
  password: !expr keyring::key_get('block_password', keyring = 'bayerChapter')
  connectionString: !expr keyring::key_get('block_connectionString', keyring = 'bayerChapter')
  cdmDatabaseSchema: !expr keyring::key_get('block_cdmDatabaseSchema', keyring = 'bayerChapter')
  vocabDatabaseSchema: !expr keyring::key_get('block_vocabDatabaseSchema', keyring = 'bayerChapter')
  workDatabaseSchema: !expr keyring::key_get('block_workDatabaseSchema', keyring = 'bayerChapter')
  role: !expr keyring::key_get('block_role', keyring = 'bayerChapter')
```

**3) Setup keyring: Have your credentials handy**

Next we need to set up the keyring for the `bayerChapter` study. Run L33:48. These lines check to see if the `bayerChapter` keyring already exists and deletes it if it does. Next, run L50. This will create a new keyring for the study.

Once we have setup the keyring we can begin inserting credentials. Run L53:68. Once L68 is run, this will prompt a dialog box that asks for you to place your credentials. Type in the credentials into the dialog box as they appear. Once you have finished check if they are correct. Run L76:84 and review your credentials. If there is any mistake, uncomment L87:88 to fix any credentials and review. As one final check, run L91:98 to ensure that the connection details can be established. If there are any errors here, please contact [martin.lavallee\@odysseusinc.com](mailto:martin.lavallee@odysseusinc.com){.email} or [george.argyriou\@odysseusinc.com](mailto:george.argyriou@odysseusinc.com){.email}.

**4) Adding other configBlocks**

With one set of credentials confirmed we must do a similar process for the remaining databases in the study. To add an additional config block to the *config.yml* file run:

```         
Ulysses::addConfig(block = "[next_block]", database = "[next_database]")
```

In this function replace the bracketed items with your next database. This function will open the *config.yml* file with a new config block set below your first one. Add the role credential as shown above for Bayer projects.

Now we need to upload the credentials of this configBlock to the keyring. Reassign the variables on L15 and L17 of the `KeyringSetup.R` file to the new block and database. Now rerun L52:69 to set the credentials in the keyring storage for your other databases. Check the credentials as demonstrated before. Rerun this step until all databases are added to the *config.yml* and keyring.

## 4) Executing Scripts

Once the credentials have been setup, the user may begin running study tasks. Navigate to the `analysis/studyTasks` folder and consider the file structure. Study tasks are ordered in the sequence that they must run based on a numerical prefix. This means you must run `01_buildCohorts.R` first in the sequence before `02_cohortDiagnostics.R`. 

### Step 1: Build Cohorts

In the `01_buildCohorts.R` replace L21 to fit the configBlock for the dbms you want to use in the study. For example if I was running my analysis on optum claims whose credentials were stored under the block *optum*, I would replace L21 with `configBlock <- "optum"`.

Once this has been changed. scroll down to L59. If you are part of Bayer, uncomment this line. If not, keep this line commented. Now you can run the script from start to finish. While the script is running, information will fill your R console describing where files have been saved and what is being done. 


### Step 2: Run Cohort Diagnostics

Next you will run the cohort diagnostics script. Follow the steps described in step 1 by replacing the assigned value for `configBlock` on L21 with that of the database you want to use.  When this is done, run the script. 

Once the cohort diagnostics function has completed, move to script `03_reviewCohortDiagnostics.R` and run it. This script will launch the shiny app to review the cohort diagnostics results. 

Once the results have been reviewed, please upload the zip file of each database found in the *02_cohortDiagnostics* folder to the aws s3 bucket. A file `extras/StoreReults.R` provides guidance on how to upload. You must contact the study lead for the aws s3 key and secret.  
