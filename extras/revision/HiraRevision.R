# JACC Asia revision: functions for the HIRA scripts (sourced automatically; do not edit).

newOutcomes <- data.frame(
  outcomeId = c(17923701, 17923702, 17923671, 17923672, 17923661, 17923662, 17923691, 17923692),
  parentId = c(1792370, 1792370, 1792367, 1792367, 1792366, 1792366, 1792369, 1792369),
  variant = c("V1", "V2", "V1", "V2", "V1", "V2", "V1", "V2"),
  outcomeName = c(
    "AMI (V1, AMI on inpatient/ER admission date)", "AMI (V2, AMI from day 7: no PCI in prior 1-6 days)",
    "MACE (V1)", "MACE (V2)", "NACE (V1)", "NACE (V2)", "Ischemic event (V1)", "Ischemic event (V2)"
  )
)
amiCohortIds <- c(original = 1792370, V1 = 17923701, V2 = 17923702)
# Components of each parent outcome (cohort IDs of the original package)
outcomeComponents <- list(
  "1792370" = c("ami"),
  "1792367" = c("ami", 1792364, 1792373, 1792372),
  "1792366" = c("ami", 1792364, 1792373, 1792372, 1792374),
  "1792369" = c("ami", 1792373)
)
exposureCohorts <- data.frame(
  cohortId = c(9990103, 9990104),
  cohortName = c("Revision: clopidogrel exposures", "Revision: exposures to the other study drug")
)

readRevisionSql <- function(fileName) {
  paste(readLines(file.path("extras", "revision", "sql", fileName), warn = FALSE), collapse = "\n")
}

# Step 1: cohort generation ---------------------------------------------------------------
revisionCohortDefinitionSet <- function(cohortDatabaseSchema, cohortTable, targetId, comparatorId,
                                        targetIngredient, comparatorIngredient) {
  ami <- data.frame(
    cohortId = amiCohortIds[c("V1", "V2")],
    cohortName = paste("Revision outcome:", newOutcomes$outcomeName[1:2]),
    sql = sapply(amiCohortIds[c("V1", "V2")], function(id) readRevisionSql(paste0(id, ".sql")))
  )
  template <- readRevisionSql("P2y12ExposureCohort.sql")
  exposureSql <- function(targetArmIngredient, comparatorArmIngredient) {
    SqlRender::render(template, study_cohort_database_schema = cohortDatabaseSchema, study_cohort_table = cohortTable,
                      target_id = targetId, comparator_id = comparatorId,
                      target_arm_ingredient_id = targetArmIngredient, comparator_arm_ingredient_id = comparatorArmIngredient)
  }
  exposures <- data.frame(
    cohortId = exposureCohorts$cohortId,
    cohortName = exposureCohorts$cohortName,
    sql = c(exposureSql(p2y12Ingredients[["clopidogrel"]], p2y12Ingredients[["clopidogrel"]]),
            exposureSql(comparatorIngredient, targetIngredient))
  )
  rbind(ami, exposures)
}

createRevisionCohorts <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  revisionCohortTable,
                                  exportFolder,
                                  incrementalFolder,
                                  databaseId,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  targetId = 1792797,
                                  comparatorId = 1793457,
                                  targetIngredient = p2y12Ingredients[["ticagrelor"]],
                                  comparatorIngredient = p2y12Ingredients[["prasugrel"]],
                                  minCellCount = 5) {
  if (toupper(revisionCohortTable) == toupper(cohortTable)) stop("revisionCohortTable must differ from cohortTable")
  dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)
  cohortTableNames <- CohortGenerator::getCohortTableNames(revisionCohortTable)
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails, cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTableNames = cohortTableNames, incremental = TRUE)
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = revisionCohortDefinitionSet(cohortDatabaseSchema, cohortTable, targetId, comparatorId,
                                                      targetIngredient, comparatorIngredient),
    incremental = TRUE,
    incrementalFolder = incrementalFolder
  )

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  message("Counting revision cohorts")
  counts <- CohortGenerator::getCohortCounts(connection = connection, cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTable = revisionCohortTable) %>%
    mutate(across(any_of(c("cohortEntries", "cohortSubjects")), ~ censorCounts(.x, minCellCount)),
           databaseId = !!databaseId, .before = 1)
  utils::write.csv(counts, file.path(exportFolder, "revision_cohort_counts.csv"), row.names = FALSE)

  abciximabFile <- file.path(exportFolder, "revision_abciximab_records.csv")
  if (file.exists(abciximabFile)) {
    message("Abciximab counts already done, skipped")
  } else {
    message("Abciximab record counts (aggregate)")
    abciximab <- DatabaseConnector::renderTranslateQuerySql(
      connection, readRevisionSql("AbciximabRecordCounts.sql"), snakeCaseToCamelCase = TRUE,
      cdm_database_schema = cdmDatabaseSchema, cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable, target_id = targetId, comparator_id = comparatorId
    )
    denominators <- CohortGenerator::getCohortCounts(connection = connection, cohortDatabaseSchema = cohortDatabaseSchema,
                                                     cohortTable = cohortTable, cohortIds = c(targetId, comparatorId)) %>%
      select(cohortDefinitionId = "cohortId", cohortSubjects = "cohortSubjects")
    abciximab <- abciximab %>%
      left_join(denominators, by = "cohortDefinitionId") %>%
      mutate(across(c("persons", "records"), ~ censorCounts(.x, minCellCount)), databaseId = !!databaseId, .before = 1)
    colnames(abciximab) <- SqlRender::camelCaseToSnakeCase(colnames(abciximab))
    utils::write.csv(abciximab, abciximabFile, row.names = FALSE)
  }
  invisible(counts)
}

# Step 2: reading the cohort DB (read-only, cohortDatabaseSchema only) -------------------
# AMI events of the study patients: V1/V2 (revision table) and the original AMI cohort
amiRowsSql <- "
SELECT CAST(o.subject_id AS VARCHAR(50)) AS person_id, o.cohort_definition_id, o.cohort_start_date
FROM @cohort_database_schema.@revision_cohort_table o
WHERE o.cohort_definition_id IN (@ami_variant_ids)
  AND o.subject_id IN (SELECT subject_id FROM @cohort_database_schema.@cohort_table
                       WHERE cohort_definition_id IN (@target_id, @comparator_id))
UNION ALL
SELECT CAST(o.subject_id AS VARCHAR(50)) AS person_id, o.cohort_definition_id, o.cohort_start_date
FROM @cohort_database_schema.@cohort_table o
WHERE o.cohort_definition_id = @ami_original_id
  AND o.subject_id IN (SELECT subject_id FROM @cohort_database_schema.@cohort_table
                       WHERE cohort_definition_id IN (@target_id, @comparator_id));"

# P2Y12 exposures: revision exposure cohorts, plus the index-drug era (the target and
# comparator cohorts end when the index drug stops, 14-day gap)
p2y12RowsSql <- "
SELECT CAST(c.subject_id AS VARCHAR(50)) AS person_id,
  c.cohort_definition_id,
  c.cohort_start_date,
  d.cohort_definition_id AS exposure_cohort_id,
  d.cohort_start_date AS drug_exposure_start_date,
  d.cohort_end_date AS drug_exposure_end_date
FROM @cohort_database_schema.@cohort_table c
INNER JOIN @cohort_database_schema.@revision_cohort_table d
  ON d.subject_id = c.subject_id
WHERE c.cohort_definition_id IN (@target_id, @comparator_id)
  AND d.cohort_definition_id IN (@exposure_cohort_ids)
UNION ALL
SELECT CAST(c.subject_id AS VARCHAR(50)) AS person_id,
  c.cohort_definition_id,
  c.cohort_start_date,
  0 AS exposure_cohort_id,
  c.cohort_start_date AS drug_exposure_start_date,
  c.cohort_end_date AS drug_exposure_end_date
FROM @cohort_database_schema.@cohort_table c
WHERE c.cohort_definition_id IN (@target_id, @comparator_id);"

readRevisionCohorts <- function(connectionDetails, cohortDatabaseSchema, cohortTable, revisionCohortTable,
                                targetId = 1792797, comparatorId = 1793457,
                                targetIngredient = p2y12Ingredients[["ticagrelor"]],
                                comparatorIngredient = p2y12Ingredients[["prasugrel"]]) {
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  query <- function(sql, ...) {
    DatabaseConnector::renderTranslateQuerySql(
      connection, sql, snakeCaseToCamelCase = TRUE,
      cohort_database_schema = cohortDatabaseSchema, cohort_table = cohortTable,
      revision_cohort_table = revisionCohortTable, target_id = targetId, comparator_id = comparatorId, ...
    )
  }
  message("Reading AMI cohorts (read-only)")
  amiRows <- query(amiRowsSql, ami_variant_ids = amiCohortIds[c("V1", "V2")], ami_original_id = amiCohortIds[["original"]])
  message("Reading P2Y12 exposure cohorts (read-only)")
  p2y12Records <- query(p2y12RowsSql, exposure_cohort_ids = exposureCohorts$cohortId) %>%
    mutate(ingredientConceptId = case_when(
      .data$exposureCohortId == 0 & .data$cohortDefinitionId == targetId ~ targetIngredient,
      .data$exposureCohortId == 0 ~ comparatorIngredient,
      .data$exposureCohortId == 9990103 ~ p2y12Ingredients[["clopidogrel"]],
      .data$cohortDefinitionId == targetId ~ comparatorIngredient,
      TRUE ~ targetIngredient
    )) %>%
    select(-"exposureCohortId")
  list(amiRows = amiRows, p2y12Records = p2y12Records)
}

# Step 2: outcomes on the saved populations -----------------------------------------------
# AMI cohort start dates as days from each cohort row's index date (as in cmData$outcomes)
amiDays <- function(amiRows, cohorts, cohortId) {
  amiRows %>%
    filter(.data$cohortDefinitionId == cohortId) %>%
    transmute(personId = idToChar(.data$personId), outcomeDate = as.Date(.data$cohortStartDate)) %>%
    inner_join(cohorts %>% select("rowId", "personId", indexDate = "cohortStartDate"), by = "personId") %>%
    transmute(.data$rowId, start = as.numeric(.data$outcomeDate - .data$indexDate), end = .data$start + 1)
}

# Circe era collapse (pad 0): intervals that overlap or touch form one era
collapseEras <- function(intervals) {
  if (nrow(intervals) == 0) return(data.frame(rowId = numeric(0), start = numeric(0)))
  intervals %>%
    arrange(.data$rowId, .data$start) %>%
    group_by(.data$rowId) %>%
    mutate(era = cumsum(c(1, .data$start[-1] > cummax(.data$end)[-n()]))) %>%
    group_by(.data$rowId, .data$era) %>%
    summarise(start = min(.data$start), end = max(.data$end), .groups = "drop")
}

# Event eras per rowId: original outcomes assembled from components ("rebuilt_<id>") and
# the V1/V2 variants
buildOutcomeEras <- function(amiRows, outcomes, cohorts) {
  ami <- lapply(amiCohortIds, function(id) amiDays(amiRows, cohorts, id))
  componentIntervals <- function(cohortId) {
    outcomes %>%
      filter(.data$outcomeId == cohortId) %>%
      transmute(.data$rowId, start = .data$daysToEvent, end = .data$daysToEvent + 1)
  }
  build <- function(parentId, amiVersion) {
    parts <- lapply(outcomeComponents[[parentId]], function(component) {
      if (component == "ami") ami[[amiVersion]] else componentIntervals(as.numeric(component))
    })
    collapseEras(bind_rows(parts))
  }
  eras <- list()
  for (parentId in names(outcomeComponents)) {
    eras[[paste0("rebuilt_", parentId)]] <- build(parentId, "original")
    for (variant in c("V1", "V2")) {
      outcomeId <- newOutcomes$outcomeId[newOutcomes$parentId == as.numeric(parentId) & newOutcomes$variant == variant]
      eras[[as.character(outcomeId)]] <- build(parentId, variant)
    }
  }
  eras
}

# Replace the outcome of a saved population (CohortMethod createStudyPopulation logic)
applyOutcome <- function(population, eras) {
  if (nrow(population) == 0) return(population)
  if (nrow(eras) == 0) eras <- data.frame(rowId = population$rowId[0], start = numeric(0))
  inWindow <- eras %>%
    inner_join(population %>% select("rowId", "riskStart", "riskEnd"), by = "rowId") %>%
    filter(.data$start >= .data$riskStart, .data$start <= .data$riskEnd) %>%
    group_by(.data$rowId) %>%
    summarise(outcomeCount = n(), daysToEvent = min(.data$start), .groups = "drop")
  population %>%
    select(-any_of(c("outcomeCount", "daysToEvent", "survivalTime"))) %>%
    left_join(inWindow, by = "rowId") %>%
    mutate(
      outcomeCount = coalesce(.data$outcomeCount, 0L),
      survivalTime = if_else(.data$outcomeCount > 0, .data$daysToEvent - .data$riskStart + 1, .data$timeAtRisk)
    )
}

estimateOf <- function(model) {
  x <- model$outcomeModelTreatmentEstimate$logRr
  if (length(x) == 0) NA_real_ else x[1]
}

runHiraRevision <- function(outputFolder,
                            exportFolder,
                            databaseId,
                            amiRows,
                            targetId = 1792797,
                            comparatorId = 1793457,
                            minCellCount = 5) {
  cmFolder <- file.path(outputFolder, "cmOutput")
  dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)
  write <- function(x, name) {
    if (is.null(x) || nrow(x) == 0) return(invisible())
    x <- x %>% mutate(databaseId = !!databaseId, .before = 1)
    colnames(x) <- SqlRender::camelCaseToSnakeCase(colnames(x))
    utils::write.csv(x, file.path(exportFolder, paste0("revision_", name, ".csv")), row.names = FALSE)
  }
  reference <- as.data.frame(readRDS(file.path(cmFolder, "outcomeModelReference.rds"))) %>%
    filter(.data$targetId == !!targetId, .data$comparatorId == !!comparatorId)
  analysisIds <- sort(unique(reference$analysisId))

  cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, reference$cohortMethodDataFile[1]))
  cohorts <- cmData$cohorts %>%
    select("rowId", "personId", "cohortStartDate") %>%
    collect() %>%
    mutate(personId = idToChar(.data$personId))
  outcomes <- cmData$outcomes %>% collect()
  Andromeda::close(cmData)

  message("Assembling outcome eras")
  eras <- buildOutcomeEras(amiRows, outcomes, cohorts)

  message("Re-estimating outcomes on the saved populations")
  modelFolder <- file.path(exportFolder, "temp_models")
  dir.create(modelFolder, showWarnings = FALSE)
  on.exit(unlink(modelFolder, recursive = TRUE), add = TRUE)
  newReference <- list()
  validation <- list()
  kmData <- list()
  primaryPopulations <- list()
  for (analysisId in analysisIds) {
    for (parentId in names(outcomeComponents)) {
      parentRow <- reference %>% filter(.data$analysisId == !!analysisId, .data$outcomeId == as.numeric(parentId))
      popFile <- if (parentRow$strataFile != "") parentRow$strataFile else parentRow$studyPopFile
      parentPopulation <- readRDS(file.path(cmFolder, popFile))
      parentModel <- readRDS(file.path(cmFolder, parentRow$outcomeModelFile))
      fit <- function(population) {
        CohortMethod::fitOutcomeModel(population = population, modelType = parentModel$outcomeModelType,
                                      stratified = parentModel$outcomeModelStratified)
      }
      # Validation: the original outcome assembled the same way must reproduce the saved estimate
      rebuilt <- applyOutcome(parentPopulation, eras[[paste0("rebuilt_", parentId)]])
      rebuiltModel <- fit(rebuilt)
      validation[[length(validation) + 1]] <- data.frame(
        analysisId = analysisId, outcomeId = as.numeric(parentId),
        savedLogRr = estimateOf(parentModel),
        rebuiltLogRr = estimateOf(rebuiltModel),
        subjectsWithSameOutcome = sum((rebuilt$outcomeCount > 0) == (parentPopulation$outcomeCount > 0)),
        subjectsWithSameDaysToEvent = sum(coalesce(rebuilt$daysToEvent, -1) == coalesce(parentPopulation$daysToEvent, -1)),
        subjects = nrow(parentPopulation)
      )
      for (outcomeId in newOutcomes$outcomeId[newOutcomes$parentId == as.numeric(parentId)]) {
        population <- applyOutcome(parentPopulation, eras[[as.character(outcomeId)]])
        model <- fit(population)
        modelFile <- sprintf("om_a%d_o%d.rds", analysisId, outcomeId)
        saveRDS(model, file.path(modelFolder, modelFile))
        newReference[[length(newReference) + 1]] <- data.frame(
          analysisId = analysisId, targetId = targetId, comparatorId = comparatorId,
          outcomeId = outcomeId, outcomeModelFile = modelFile
        )
        # As in the original export: no KM for empty populations or without shared strata
        km <- if (nrow(population) == 0) NULL else tryCatch(prepareKaplanMeier(population), error = function(e) NULL)
        if (!is.null(km)) {
          kmData[[length(kmData) + 1]] <- km %>%
            mutate(targetId = targetId, comparatorId = comparatorId, outcomeId = outcomeId, analysisId = analysisId)
        }
        if (analysisId == 8) primaryPopulations[[as.character(outcomeId)]] <- population
      }
    }
  }
  validation <- bind_rows(validation)
  write(validation, "hira_validation")

  message("Summarising and calibrating with the existing negative controls")
  summary <- CohortMethod::summarizeAnalyses(bind_rows(newReference), modelFolder)
  analysisSummary <- utils::read.csv(file.path(outputFolder, "analysisSummary.csv"))
  negativeControlIds <- unique(reference$outcomeId[!reference$outcomeOfInterest])
  allControls <- data.frame(targetId = targetId, comparatorId = comparatorId,
                            outcomeId = negativeControlIds, targetEffectSize = 1)
  results <- bind_rows(lapply(analysisIds, function(analysisId) {
    controls <- analysisSummary %>%
      filter(.data$analysisId == !!analysisId, .data$targetId == !!targetId, .data$comparatorId == !!comparatorId,
             .data$outcomeId %in% negativeControlIds)
    subset <- bind_rows(summary %>% filter(.data$analysisId == !!analysisId),
                        controls[, intersect(colnames(controls), colnames(summary))])
    calibrate(subset, allControls) %>% filter(.data$outcomeId %in% newOutcomes$outcomeId)
  }))
  results <- results %>% mutate(databaseId = !!databaseId)
  for (field in c("targetSubjects", "comparatorSubjects", "targetOutcomes", "comparatorOutcomes")) {
    results <- enforceMinCellValue(results, field, minCellCount, silent = TRUE)
  }
  results <- results %>% select(-"databaseId")
  write(results, "cohort_method_result")
  write(newOutcomes, "outcome_definitions")

  kmData <- bind_rows(kmData)
  if (nrow(kmData) > 0) {
    kmData <- enforceMinCellValue(kmData, "targetAtRisk", minCellCount, silent = TRUE)
    kmData <- enforceMinCellValue(kmData, "comparatorAtRisk", minCellCount, silent = TRUE)
  }
  write(kmData, "kaplan_meier_dist")

  list(primaryPopulations = primaryPopulations, validation = validation)
}
