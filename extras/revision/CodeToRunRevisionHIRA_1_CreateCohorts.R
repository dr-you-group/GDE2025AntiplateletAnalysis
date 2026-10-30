# JACC Asia revision, HIRA step 1 of 2: create revision cohorts.
# Fill in the settings and source this file. If it stops, source it again.
library(dplyr)
source("extras/revision/RevisionPostHoc.R")
source("extras/revision/HiraRevision.R")

# Settings: same as the original run ---------------------------------------------------
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
cohortTable <- ""
outputFolder <- ""
options(sqlRenderTempEmulationSchema = NULL)

# No changes needed below -----------------------------------------------------------------
revisionCohortTable <- paste0(cohortTable, "_rev")
createRevisionCohorts(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  revisionCohortTable = revisionCohortTable,
  exportFolder = file.path(outputFolder, "revisionHIRA"),
  incrementalFolder = file.path(outputFolder, "revisionHIRA_incremental"),
  databaseId = "HIRA",
  minCellCount = 5
)
