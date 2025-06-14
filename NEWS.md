# EHDEN HMB v2.4.2

* Fixed cohort definitions of `hormonalLngIUD`, `undefinedIUD` and `copperIUDprocedure` (cohort exit strategy)
* Updated `disorderOfovary` cohort definition with correct concept set on index date event

# EHDEN HMB v2.4.1

* Updated cohort definition `hormonalLngIUD` with new concept set

# EHDEN HMB v2.4.0

* Added scripts and functions for additional task (pre/post prevalence of new hormonal Lng IUD cohort, csv files from CohortDiagnostics for flowchart)

# EHDEN HMB v2.3.5

* Added database name of data partner in README.md file

# EHDEN HMB v2.3.4

* Added check in function `getTteResKM`(_procedureAnalysis.R). If no data are returned, skip export and subsequent data transformation.

# EHDEN HMB v2.3.3

* Edited `hmb` cohort definition. Added Observation domain block to Cohort Entry Events because of a change of concept `4302555` from Condition to Observation.

# EHDEN HMB v2.3.2

* Added code to remove double quotes from credentials in order to connect to server with DBI (function: `cdmFromConAllDbs`)

# EHDEN HMB v2.3.1

* Added Redshift connectivity in function `cdmFromConAllDbs`
* Updated the `KeyringSetup.R` file to add two additional credentials: port and server 

# EHDEN HMB v2.3.0

* Added function `cdmFromConAllDbs` to connect to different SQL servers with ODBC drivers

# EHDEN HMB v2.2.0

* Added `if-clause` for Snowflake specific SQL functions
* Added code that checks if the output of function `FeatureExtraction::getDbCovariateData` is empty

# EHDEN HMB v2.1.3

* Updated renv.lock

# EHDEN HMB v2.1.2

* Edited info text in `About` page
* Renamed age group label from `age_30_45` to `age_30_44`
* Added task to create png files of the KM plots for TTD and TTI

# EHDEN HMB v2.1.1

* Removed concept set `undefinedIUD` from analysis

# EHDEN HMB v2.1.0

* Moved `hormonalLngIud` concept from `Procedure Analysis` to `Treatment Patterns`
* Edited info text in `Treatment Patterns` section

# EHDEN HMB v2.0.0

* Updated `endometriosis`, `disorder of ovary`, `myomectomy`, `hysterectomy` `endometrial ablation` and `endometrial polyp` concept sets
* Merged concept sets `hormonal IUD` and `lng IUD` into one concept set named `hormonalLngIud`
* Added new covariate concept set `endometriosis of uterus`
* Added procedures in the `Treatment Patterns` analysis (Sequences)
* Edited `Treatment Patterns` function code to include event cohorts that started the same day as the target cohort's end date
* Reworked incidence analysis. Using the `IncidencePrevalenc`e package to calculate.
* Added `Age distribution` plot in shiny app section `Clinical Characteristics`
* Added new time window (at index i.e. 0 days from index date) for drug and procedure prevalence analysis
* Changed the label of the first age group from `0-30` to `11-29`
* Created additional task named `zip results`

# EHDEN HMB v1.1.1

* Adjusted height and width parameters for KM plots pictures in the shiny app

# EHDEN HMB v1.1.0

* Reworked incidence analysis. Age groups' incidence is calclulated by the CohortIncidence package and not by stratified cohorts.

# EHDEN HMB v1.0.2

* Fixed cohort picker in shiny app
* Updated renv.lock file for package dependency

# EHDEN HMB v1.0.1

* Corrected NEWS.md file
* Added "result"" and "data"" folders in .gitignore file

# EHDEN HMB v1.0.0

* Removed `HMB2` cohort definition
* Added all drug cohorts to post-index utilization analysis
* Split Oral Contraceptives concept set to 1) All oral contraceptives excluding estradiol+dienogest and 2) just estradiol+dienogest oral contraceptives
* Added narrow definition of pain covariate (i.e. pain associated with HMB)
* Added new Treatment Patterns analysis (without NSAIDs). Kept the initial analysis as sensitivity (with NSAIDs).
* Added new Time to discontinuation analysis (without NSAIDs). Kept the initial analysis as sensitivity (with NSAIDs).
* Added risk table with people at risk and censored underneath KM plots
* Added subsets of Treatment Patterns analysis i.e. patterns of first 6 months, 1 year and 2 years after index date
* Drug utilization has been extended to two different analyses: 'Within time window' and 'Complete follow-up'
* Incidence analysis has been altered. Incidence rate is calculated by using the same stratified cohorts in both numerator and denominator

# EHDEN HMB v0.9.0

* Updated `.gitignore` with Github R template
* Corrected links in `README.md` file
* Corrected database names in `sap.qmd` and `index.qmd` file
* Tidied up R scripts
* Replaced SQL function `EXTRACT` with `YEAR` in the `_buildStrata.R` script to translate SQL code to Azure Synapse dialect
* Converted `dbplyr` function to SQL code
* Reworked `initializeCohortTables` function (Added `drop table` sql code)
* Added database information Markdown in shiny app


# EHDEN HMB v0.2.0

* Add shiny app to preview results

# EHDEN HMB v0.1.8

* Fix pid and std file names in cohort definitions, changes anticipated build order

# EHDEN HMB v0.1.7

* Correct event in time to procedure intervention

# EHDEN HMB v0.1.6

* Minor bug fix to treatment patterns analysis
* Correct typo in functions

# EHDEN HMB v0.1.5

* Correct bug in analysis settings (Issue #12)
* Update package website
* Add yearly incidence to `Incidence Analysis` (Issue #13)
* Add R scripts for data exchange and CD preview (Issue #14)
* Update cohort definitions from Siir
    - copperIUDproc
    - copperIUDdrug
    - disorderOfOvary

# EHDEN HMB v0.1.4

* Fix bug in baseline characteristics; typo in analysis settings (Issue #10)
* Corrections to the study website


# EHDEN HMB v0.1.3

* Update cohort definitions
    - add baseline procedures
    - add baseline drugs
* Include procedure prevalence at baseline [-365,0]

# EHDEN HMB v0.1.2

* Update cohort definitions
    - rerun drug exposure with Capr
    - rerun procedures with Capr
    - add time restraints to nsaids
    - add hmb cohort without hysterectomy censor
* Add secondary treatment sequence including procedures 

# EHDEN HMB v0.1.1

* Add study website
* Update cohort definitions for denominator
* Addition of executeStudy.R file to automate execution
* Add age strata [under 30, 30-45, 45-55]

# EHDEN HMB v0.1.0

* First release of study code for EHDEN HMB
    * Add Incidence Analysis
    * Add Baseline Characteristics 
    * Add Treatment Patterns
    * Add Procedure Analysis

# EHDEN HMB v0.0.6

* Add file `StoreResults` to upload cohort diagnostics zip to aws s3 bucket
* Update `KeyringSetup` to match `Ulysses`
* Update renv.lock to add `aws.s3` and `Ulysses` v0.0.2
* Minor correction to study task files

# EHDEN HMB v0.0.5

* Added in `renv`
* Minor correction to HMB cohort for cohort diagnostics

# EHDEN HMB v0.0.4

* Fix errors in KeyringSetup.R file
* Fix cohort diagnostics to use integer64 with id in cohortDefinitionSet

# EHDEN HMB v0.0.3

* Update How to run file with installation instructions for usethis and Ulysses
* Fix extras/KeyringSetup.R file to correctly set up credentials and check them
* Edit `startSnowflakeSession` function to handle string split of workDatabaseSchema
* Edit study tasks to order snowflake start correctly and close connection at end of file

# EHDEN HMB v0.0.2

* Update cohort definition for HMB to fix error
* Add cohort diagnostics script
* Add `HowToRun` file and `keyring` file

# EHDEN HMB v0.0.1

* Add HMB cohort definition to repo
* Start preparing documentation about study
* Initialize OHDSI study
* Add `NEWS.md` to track changes
