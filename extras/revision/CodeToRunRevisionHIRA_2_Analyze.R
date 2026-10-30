# JACC Asia revision, HIRA step 2 of 2: analysis. Run after step 1.
# Fill in the settings and source this file. Send the <outputFolder>/revisionHIRA folder.
library(dplyr)
source("R/Export.R")
source("extras/revision/RevisionPostHoc.R")
source("extras/revision/HiraRevision.R")

# Settings: same as step 1 -------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "",
  server = "",
  user = "",
  password = "",
  port = "",
  pathToDriver = ""
)
cohortDatabaseSchema <- ""
cohortTable <- ""
outputFolder <- ""
options(sqlRenderTempEmulationSchema = NULL)

# No changes needed below -----------------------------------------------------------------
revisionCohortTable <- paste0(cohortTable, "_rev")
databaseId <- "HIRA"
minCellCount <- 5
exportFolder <- file.path(outputFolder, "revisionHIRA")
cohortRows <- readRevisionCohorts(connectionDetails, cohortDatabaseSchema, cohortTable, revisionCohortTable)
cmAnalysisList <- CohortMethod::loadCmAnalysisList("inst/settings/cmAnalysisList.json")
cmAnalysis <- function(analysisId) Filter(function(a) a$analysisId == analysisId, cmAnalysisList)[[1]]

hira <- runHiraRevision(
  outputFolder = outputFolder,
  exportFolder = exportFolder,
  databaseId = databaseId,
  amiRows = cohortRows$amiRows,
  minCellCount = minCellCount
)

runRevisionPostHoc(
  cmFolder = file.path(outputFolder, "cmOutput"),
  exportFolder = exportFolder,
  databaseId = databaseId,
  primaryAnalysisId = 8,
  onTreatmentAnalysisId = 2,
  createStudyPopArgs = list("8" = cmAnalysis(8)$createStudyPopArgs, "2" = cmAnalysis(2)$createStudyPopArgs),
  stratifyByPsArgs = cmAnalysis(8)$stratifyByPsArgs,
  populations = hira$primaryPopulations[c("17923671", "17923672")],
  p2y12Records = cohortRows$p2y12Records,
  minCellCount = minCellCount
)
