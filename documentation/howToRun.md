# Setup Study


## Download Zip

To avoid using git to leverage this study, the easiest way to access the study code is via a zip file. Instructions for downloading the zip file are below:

1)  Navigate to github [repo url](https://github.com/OdyOSG/ehden_hmb).
2)  Select the green code button revealing a dropdown menu.
3)  Select *Download Zip*.
4)  Unzip the folder on your local computer that is easily accessible within R Studio.
5)  Open the unzipped folder and select `ehden_hmb.Rproj`.


## Setup R Environment

### Using `renv`

This study uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) to reproduce the R environment to execute this study. The study code maintains an `renv.lock` file in the main branch of the repository. To activate the R dependencies through `renv` use the following code:

``` r
renv::restore()
```

### Troubleshooting `renv`

Sometimes there are errors in the package installation via `renv`. If you encounter an error try removing the problematic package from the `renv.lock` file and restore again. To remove a package from the lock file find the header of the package and delete all corresponding lines. Once you are able to get the remaining packages to install, manually install the problem package using one of the strategies below:

``` r

# Installing an R package from CRAN ------------

## Installing latest version of R package on CRAN
install.packages("ggplot2")

## Installing archived version of R package on CRAN
packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_0.9.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

# Installing an R package from github -----------------

# Installing current version of R package from github
install.packages("remotes") # note you may also work devtools
remotes::install_github("ohdsi/FeatureExtraction")

# Installing develop version of R package from github
remotes::install_github("ohdsi/Ulysses", ref = "develop")

# Installing old version of R package from github
remotes::install_github("ohdsi/CohortGenerator", ref = "v0.7.0")

```

Any additional issues with the `renv` lock file please file an [issue](https://github.com/OdyOSG/ehden_hmb/issues/new?assignees=mdlavallee92&labels=bug&projects=&template=bug_report.md&title=) in the study repository.

### Conflicts

Some organizational IT setups pose conflicts with `renv`. One example is if your organization uses the Broadsea Docker. If you are aware that your OHDSI environment will conflict with `renv`, deactivate it by running `renv::deactivate()` in the active ehden .RProj. Also delete the renv folder in your project directory. Review the list of R packages required to run the study in the **Technical Requirements** tab of the study hub and manually install.

It is **highly recommended** you stick with the `renv` snapshot as this is the easiest way to reproduce the study execution environment. Please only deactivate the lock file if it is a last resort.

## Load Execution Credentials

This study uses `keyring` and `config` to mask and query database credentials needed for execution. The study will help load these credentials using the file `extras/KeyringSetup.R`.


### Required Credentials

Data nodes executing this study require the following credentials:

1) **dbms** - the name of the dbms you are using (redshift, postgresql, snowflake, etc)
2) **user** - the username credential used to connect to the OMOP database
3) **password** - the password credential used to connect to the OMOP database
4) **connectionString** - a composed string that establishes the connection to the OMOP database. An example of a connection string would be *jdbc:dbms://host-url.com:port/database_name*.
5) **cdmDatabaseSchema** - the database and schema used to access the cdm. Note that this credential may be separated by a dot to indicate the database and schema, which tends to be the case in sql server. Example: *our_cdm.cdm*.
5) **vocabDatabaseSchema** - the database and schema used to access the vocabulary tables. Note this is typically the same as the cdmDatabaseSchema.
5) **workDatabaseSchema** - a section of the database where the user has read/write access. This schema is where we write the cohortTable used to enumerate the cohort definitions.

It is recommended that you write these credentials down in a private txt file to make it easier to load into the credential manager for the study.


### Loading Credentials

1) Open the file in the study named `extras/KeyringSetup.R`.
2) On L15:16 place a name for your config block and the database. The configBlock name can be an abbreviation, for example:

 ``` r
configBlock <- "synpuf"
database <- "synpuf_110k"
```

3) One at a time run each line in the script and follow any prompts
    - L27 asks you to build a new config.yml file which can be done as so `Ulysses::makeConfig(block = configBlock, database = database)`. Running this function will open a new file. Make sure any keyring variables is labelled as **ehden_hmb**.
    - L32 will setup a new `keyring` for the study. The name of the keyring is **ehden_hmb** and the password for the keyring is **ohdsi**. If R prompts you to place a keyring password it will be **ohdsi** unless changed by the user.
    - L36 will setup all the credentials for the study. A prompt will appear asking to input credentials, using your txt file input your credentials into the prompt. Review the credentials once you are done

#### Troubleshooting

If you have a problem with the keyring, please post an [issue](https://github.com/OdyOSG/ehden_hmb/issues/new?assignees=mdlavallee92&labels=bug&projects=&template=bug_report.md&title=) in the study repository. Otherwise you can avoid the keyring api by hard-coding your credentials to the `config.yml` file as shown in the example:

``` yml
db: # replace with an acronym for your database no underscore
  databaseName: db_ehr # replace with the name of your database
  dbms: <your_dbms>
  user:  <your_user>
  password:  <your_pass>
  connectionString: <your_connectionString>
  cdmDatabaseSchema:  <your_ cdmDatabaseSchema>
  vocabDatabaseSchema:  <your_ vocabDatabaseSchema>
  workDatabaseSchema: <your_ workDatabaseSchema>
  cohortTable: ehden_hmb_<databaseName>

```

# Run Study

## Execution Script

Running all tasks sequentially can be done using the `executeStudy.R` file. Replace L17 with the configBlock of choice and run the script.


## Study Tasks

Once your study is setup you are ready to run the EHDEN HMB study. The EHDEN HMB study contains six analytical tasks:

1) **Build Cohorts** - initialize a cohort table and generate cohort counts from the circe json in cohortsToCreate
2) **Cohort Diagnostics** - review of the hmb cohort definition
3) **Incidence Analysis** - calculation of the incidence of hmb in a population of females between ages 11 to 55
4) **Baseline Characteristics** - generation of prevalence of demographics and comorbidities at baseline (-365d prior to index)
5) **Treatment Patterns** - calculation of drug prevalence post-index, sequences and time to treatment discontinuation
6) **Procedure Analysis** - calculation of post-index prevalence of procedures and time to initial treatment

Each task will output to the `results` folder. **Note** that the first task *buildCohorts* is required to run any additional analytical task (base dependency). If a cohort definition json has changed it requires rerunning the *buildCohorts* task.


## Review Cohort Diagnostics

If you would like to review the results of the Cohort Diagnostics prior to sending you can do so by the instructions below:

1) Open file `extras/ExploreDiagnostics.R`
2) Run the R file line by line
    - Replace L20 with the name of your database
    - L28 will create a new folder in the directory called `scratchDiagnostics` where it will save the sqlite object
    - L36 prepares the diagnostics results per database
    - L41 launches the shiny app
3) Ignore any warning messages about reserved keywords and navigation containers


## Results Folder

Following successful execution of the study, each database will have its own sub-folder within results. There will be subfolders containing results from the execution underneath the database. Within each of these folders there will be a combination of `csv`, `rds` and `parquet` files that contain the results. You may review these files individually if you wish. Instructions on how to share the results can be found below.

## Sending Results

Results from this study will be loaded to a secure file transfer protocol (sftp) hosted by Bayer. Participants in the study will be provided a key and password for the sftp emailed individually. Please contact [Marta Pineda Moncusi](mailto:marta.pinedamoncusi@ndorms.ox.ac.uk) if you do not have the sftp credentials prior to running the study.
Once script `executeStudy.R` is ran successfully, a zip file will be created in the root directory of the R project called `results_<databaseName>.zip`. This is the file that you will need to share with the study coordinators (see instructions below).

### Instructions to Share

#### SFTP

1) Review that a sftp key and secret credential have been sent to your site via email
2) Open file `extras/ShareResults.R`
3) Run the R file line by line
    - When running L30 you will be asked to input the sftp credentials in a prompt
    - Copy and paste the credentials into the prompt
    - Replace L37 with a shortname for your study site
    - Execute the remaining lines
4) Contact the study coordinators that you have successfully uploaded your results to the sftp server

#### Via email

You may also email the results to [George Argyriou](mailto:george.argyriou@odysseusinc.com) as a zip file using the following naming convention: `<site_id>_results_<date>.zip`.

