# Set a library path if needed. 
.libPaths('C:/Program Files/R/library_estimation')

# R environment restoration/inspection: Run renv::restore first.
renv::restore()
renv::status()
renv::deactivate()

library(GDE2025AntiplateletAnalysis)

# Specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "C:/andromedaTemp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores() - 1

# The folder where the study intermediate and result files will be written:
outputFolder <- "C:/Users/paul9/Rprojects/GDE2025AnalysisResult2"

# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
                                                                user = "paul9567",
                                                                password = "Euez3yz!@#",
                                                                server = "10.19.10.241")
# Check connection
conn <- connect(connectionDetails) 

# Replace with the name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "CDM_v531_YUHS.CDM"

# Replace with the name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "cohortdb.changhoonhan"
cohortTable <- "GDE2025AntiplateletAnalysis8"

# Some meta-information that will be used by the export function:
databaseId <- "YUHS"
databaseName <- "YUHS"
databaseDescription <- "YUHS"

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
        verifyDependencies = FALSE, # Set to FALSE. Not needed after renv::restore is done.
        createCohorts = FALSE,
        synthesizePositiveControls = FALSE,
        runAnalyses = FALSE,
        packageResults = TRUE,
        maxCores = maxCores)


resultsZipFile <- file.path(outputFolder, "export", paste0("Results_", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")

# You can inspect the results if you want:
# Preparing for evidence explorer
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)

# Launching shiny app: Requires 'shiny' package. 
# Recommended to lauch after renv::deactivate, on a separate library with 'shiny' package installed.
launchEvidenceExplorer(dataFolder = dataFolder, blind = FALSE, launch.browser = FALSE) 



# Use email to share your resultsZipFile (paul9567@yuhs.ac)
