# JACC Asia revision: HIRA add-on analyses ---------------------------------------------
# 1. Open the GDE2025AntiplateletAnalysis project folder in the R/renv environment of the
#    original run.
# 2. Fill in the settings below with the values of the original run.
# 3. Source this file. Output: <outputFolder>/revisionHIRA/*.csv (aggregate; counts below
#    5 shown as -5). Please send that folder.
# Needs: <outputFolder>/cmOutput (read only) and the original cohort table in the database.
# Database: four read-only SELECT queries; no tables created, nothing written.
library(dplyr)
source("R/Export.R")
source("extras/revision/RevisionPostHoc.R")
source("extras/revision/HiraRevision.R")

# Settings: same as the original run ------------------------------------------------------
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

# No changes needed below -----------------------------------------------------------------
exportFolder <- file.path(outputFolder, "revisionHIRA")
cmAnalysisList <- CohortMethod::loadCmAnalysisList("inst/settings/cmAnalysisList.json")
cmAnalysis <- function(analysisId) Filter(function(a) a$analysisId == analysisId, cmAnalysisList)[[1]]

# 1. AMI-variant outcomes in all 8 analyses, validation, abciximab check
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

# 2. Subgroups, baseline severity, zero time at risk, study period, switching
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
