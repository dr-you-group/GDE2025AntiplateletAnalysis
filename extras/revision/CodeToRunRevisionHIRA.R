# JACC Asia revision: HIRA add-on (TP-DEC-0015) -------------------------------------
# Run once, from the GDE2025AntiplateletAnalysis project folder, in the SAME R/renv
# environment and with the SAME outputFolder as the original study run.
#
# - Uses the saved CohortMethod files in <outputFolder>/cmOutput (not modified).
# - Database access is READ-ONLY: four SELECT queries (AMI records, existing outcome
#   cohort entries, abciximab record counts, P2Y12 records). No tables are created and
#   nothing is written to the database. The original cohort table must still exist.
# - Results are aggregate CSV files in <outputFolder>/revisionHIRA; counts below
#   minCellCount are censored. Please send that whole folder.
library(dplyr)
source("R/Export.R") # calibrate(), prepareKaplanMeier(), enforceMinCellValue() of the original run
source("extras/revision/RevisionPostHoc.R")
source("extras/revision/HiraRevision.R")

# Settings: identical to the original run ------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "",
  server = "",
  user = "",
  password = "",
  port = "",
  pathToDriver = ""
)
cdmDatabaseSchema <- ""
cohortDatabaseSchema <- ""
cohortTable <- "" # cohort table of the original run
outputFolder <- "" # outputFolder of the original run (contains cmOutput)
databaseId <- "HIRA"
minCellCount <- 5
options(sqlRenderTempEmulationSchema = NULL)

exportFolder <- file.path(outputFolder, "revisionHIRA")
cmAnalysisList <- CohortMethod::loadCmAnalysisList("inst/settings/cmAnalysisList.json")
cmAnalysis <- function(analysisId) Filter(function(a) a$analysisId == analysisId, cmAnalysisList)[[1]]

# 1. New AMI-based outcomes in all 8 analyses, calibrated; validation; abciximab ---------
hira <- runHiraRevision(
  outputFolder = outputFolder,
  exportFolder = exportFolder,
  databaseId = databaseId,
  cmAnalysisList = cmAnalysisList,
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  minCellCount = minCellCount
)

# 2. Subgroups, severity/ACS balance, zero time at risk, study period, switching ---------
runRevisionPostHoc(
  cmFolder = file.path(outputFolder, "cmOutput"),
  exportFolder = exportFolder,
  databaseId = databaseId,
  primaryAnalysisId = 8, # ITT 1 year, PS stratification
  onTreatmentAnalysisId = 2, # on-treatment, PS stratification
  createStudyPopArgs = list("8" = cmAnalysis(8)$createStudyPopArgs, "2" = cmAnalysis(2)$createStudyPopArgs),
  stratifyByPsArgs = cmAnalysis(8)$stratifyByPsArgs,
  populations = hira$primaryPopulations[c("17923671", "17923672")], # MACE-V1, MACE-V2
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  minCellCount = minCellCount
)
