# How to Run Ehden Hmb

## Pre-Study Technical Requirements

To run this study you must setup the [HADES environment](https://ohdsi.github.io/Hades/rSetup.html).

-   [R](https://cloud.r-project.org/) installed, (version 4.0 or greater)
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

## Setting up the Ehden Hmb study locally

### Get Dependencies

To run the ehden_hmb study the user must install the following packages:

```r
install.packages('usethis')
install.packages('remotes')
remotes::install_github("ohdsi/Ulysses", ref = "develop")
```

### Get Repo

1)  Go to the github url: <https://github.com/OdyOSG/ehden_hmb>
2)  Select the green code button revealing a dropdown menu
3)  Select download zip
4)  Unzip the folder on your local computer that is easily accessible within R Studio
5)  Open folder the folder and select `ehden_hmb.Rproj`

### Setup credentials

#### Create config.yml

Once you have cloned the repo into local, now you need to add in a config.yml file to specify the credentials to the OMOP db. To do this run `picard::makeConfig(block = "<block>", database = "<database>")`. In the block argument put the short name you want to use to refer to your database. For example, if you are working with optum claims you can name the block *optum*. In the database specify the full name you want to use to refer to the database. For example, if you are working with optum claims you can name the block *Optum_Claims_31_03_2022*. Please do not use spaces in any naming.

#### Setup keyring

To set up the keyring for your credentials open the file *extras/KeyringSetup.R* which walks through how to set the keyrings for the config file. This code will allow you to input your credential interactively so as not to save in a script.

## How to execute the Ehden Hmb study

To run the Ehden Hmb study, the user must execute the analysis scripts one at a time per database. Automations to this process are not yet available.

Begin by going to the *analysis/studyTasks* folder and opening the first file in the sequence. Replace the value in the configBlock to that of the configuration you wish to use. Once this has been set you may begin running the script. Please contact [*martin.lavallee\@odysseusinc.com*](mailto:martin.lavallee@odysseusinc.com){.email} if there are any issues.
