# EHDEN HMB v0.1.1

* Add study website
* Update cohort definitions for denominator


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
* minor correction to study task files

# EHDEN HMB v0.0.5

* Add in renv
* minor correction to HMB cohort for cohort diagnostics

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
* Add How to Run file and key ring file

# EHDEN HMB v0.0.1

* Add HMB cohort definition to repo
* Start preparing documentation about study
* Initialize OHDSI study
* Add `NEWS.md` to track changes to OHDSI study
