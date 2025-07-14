GDE2025AntiplateletAnalysis
==============================

Information
===========

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Clinical Application**
- Study lead: **Chang Hoon Han, MD, Seng Chan You, MD, PhD**
- Protocol: **See documents**
- Publications: **-**

Requirements
============

- A database in [Common Data Model version 5](https://ohdsi.github.io/CommonDataModel/) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, Spark, or Microsoft APS.
- R version 4.0.0 or newer (4.1 ~ 4.2 versions desired)
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java. 

2. To get started, download the study code in this repository to your local machine. Instructions for downloading are found [here](https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives#downloading-source-code-archives-from-the-repository-view). 
In this guide, we will assume that you have
downloaded the .ZIP archive to **`D:/git/ohdsi-studies/<YourNetworkStudyName>`.**

3. [Use RStudio to open the project file](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects#:~:text=There%20are%20several%20ways%20to,Rproj). `GDE2025AntiplateletAnalysis.Rproj` which is found in 
the root directory of wherever you opted to download the study package. 

The first time you run this study, you will need to restore the execution
environment using [renv](https://rstudio.github.io/renv/). To do this,
in the console run:

`renv::restore()`

and follow the prompts to install all of the packages. This will take some time
to complete (approximately 30 minutes). Once this operation is complete, please
close RStudio and re-open the project. You will again see the message above stating that packages recorded in the lockfile are not installed. You can safely 
ignore this message moving forward. At this point you are ready to run the study.

4. Once installed, you can execute the study by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`:

    ```r
    library(GDE2025AntiplateletAnalysis)

    # Optional: specify where the temporary files (used by the Andromeda package) will be created:
    options(andromedaTempFolder = "s:/andromedaTemp")
	
    # Maximum number of cores to be used:
    maxCores <- parallel::detectCores()
	
    # Minimum cell count when exporting data:
    minCellCount <- 5
	
    # The folder where the study intermediate and result files will be written:
    outputFolder <- "c:/GDE2025AntiplateletAnalysis"
	
    # Details for connecting to the server:
    # See ?DatabaseConnector::createConnectionDetails for help
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcr"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))

    # The name of the database schema where the CDM data can be found:
    cdmDatabaseSchema <- "cdm_truven_mdcr_v1911"

    # The name of the database schema and table where the study-specific cohorts will be instantiated:
    cohortDatabaseSchema <- "scratch_mschuemi"
    cohortTable <- "estimation_skeleton"

    # Some meta-information that will be used by the export function:
    databaseId <- "IBM_MDCR"
    databaseName <- "IBM MarketScan® Medicare Supplemental and Coordination of Benefits Database"
    databaseDescription <- "IBM MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans.  These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."

    # For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
    options(sqlRenderTempEmulationSchema = NULL)

    execute(connectionDetails = connectionDetails,
            cdmDatabaseSchema = cdmDatabaseSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cohortTable = cohortTable,
            outputFolder = outputFolder,
            databaseId = databaseId,
            databaseName = databaseName,
            databaseDescription = databaseDescription,
            verifyDependencies = TRUE,
            createCohorts = TRUE,
            synthesizePositiveControls = TRUE,
            runAnalyses = TRUE,
            packageResults = TRUE,
            maxCores = maxCores)
    ```

4. Send the results to the study coordinator via email: paul9567@yuhs.ac


5. To view the results, use the Shiny app:

	```r
	prepareForEvidenceExplorer("Result_<databaseId>.zip", "/shinyData")
	launchEvidenceExplorer("/shinyData", blind = TRUE)
	```
  
  Note that you can save plots from within the Shiny app. It is possible to view results from more than one database by applying `prepareForEvidenceExplorer` to the Results file from each database, and using the same data folder. Set `blind = FALSE` if you wish to be unblinded to the final results.

License
=======
The GDE2025AntiplateletAnalysis package is licensed under Apache License 2.0

Development
===========
GDE2025AntiplateletAnalysis was developed in ATLAS and R Studio.

