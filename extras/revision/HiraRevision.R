# JACC Asia revision: functions for the HIRA add-on. Sourced by CodeToRunRevisionHIRA.R;
# no need to edit. Reuses the saved cmOutput (no new cohorts, no PS refit) with three
# read-only queries: AMI records, existing outcome cohort entries, abciximab counts.
# New outcomes (as in the Strategus package) for AMI, MACE, NACE, ischemic composite:
#   V1: AMI recorded on the start date of an inpatient/ER visit
#   V2: inpatient/ER AMI with no PCI 1-6 days before it (AMI from day 7)
# Composites are rebuilt as Circe does (component events collapsed into eras; first era
# start in the time at risk). revision_hira_validation.csv checks that the original
# outcomes rebuilt this way reproduce the saved estimates.

newOutcomes <- data.frame(
  outcomeId = c(17923701, 17923702, 17923671, 17923672, 17923661, 17923662, 17923691, 17923692),
  parentId = c(1792370, 1792370, 1792367, 1792367, 1792366, 1792366, 1792369, 1792369),
  variant = c("V1", "V2", "V1", "V2", "V1", "V2", "V1", "V2"),
  outcomeName = c(
    "AMI (V1, AMI on inpatient/ER admission date)", "AMI (V2, AMI from day 7: no PCI in prior 1-6 days)",
    "MACE (V1)", "MACE (V2)", "NACE (V1)", "NACE (V2)", "Ischemic event (V1)", "Ischemic event (V2)"
  )
)
# Components of each parent outcome (cohort IDs of the original package)
outcomeComponents <- list(
  "1792370" = c("ami"),
  "1792367" = c("death", "ami", 1792373, 1792372),
  "1792366" = c("death", "ami", 1792373, 1792372, 1792374),
  "1792369" = c("ami", 1792373)
)
deathCohortId <- 1792364
amiCohortId <- 1792370

# Read-only queries ---------------------------------------------------------------------
amiRecordsSql <- "
WITH study_persons AS (
  SELECT subject_id, MIN(cohort_start_date) AS first_index_date
  FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id IN (@target_id, @comparator_id)
  GROUP BY subject_id
),
ami_concepts AS (
  SELECT ca.descendant_concept_id AS concept_id
  FROM @cdm_database_schema.concept_ancestor ca
  INNER JOIN @cdm_database_schema.concept c ON c.concept_id = ca.descendant_concept_id
  WHERE ca.ancestor_concept_id = 4329847
    AND (c.invalid_reason IS NULL OR c.concept_id = 4329847)
    AND ca.descendant_concept_id NOT IN (
      SELECT descendant_concept_id FROM @cdm_database_schema.concept_ancestor WHERE ancestor_concept_id = 314666
    )
),
visit_concepts AS (
  SELECT ca.descendant_concept_id AS concept_id
  FROM @cdm_database_schema.concept_ancestor ca
  INNER JOIN @cdm_database_schema.concept c ON c.concept_id = ca.descendant_concept_id
  WHERE ca.ancestor_concept_id IN (9201, 9203)
    AND (c.invalid_reason IS NULL OR c.concept_id IN (9201, 9203))
),
pci_concepts AS (
  SELECT concept_id FROM @cdm_database_schema.concept WHERE concept_id IN (44789455, 4337738)
  UNION
  SELECT ca.descendant_concept_id AS concept_id
  FROM @cdm_database_schema.concept_ancestor ca
  INNER JOIN @cdm_database_schema.concept c ON c.concept_id = ca.descendant_concept_id
  WHERE ca.ancestor_concept_id IN (4264286, 4006788, 4139198, 4283892)
    AND (c.invalid_reason IS NULL OR c.concept_id IN (4264286, 4006788, 4139198, 4283892))
),
ami AS (
  SELECT DISTINCT co.person_id,
    co.condition_start_date,
    op.observation_period_start_date AS op_start_date,
    op.observation_period_end_date AS op_end_date
  FROM @cdm_database_schema.condition_occurrence co
  INNER JOIN study_persons sp
    ON sp.subject_id = co.person_id
  INNER JOIN @cdm_database_schema.observation_period op
    ON op.person_id = co.person_id
    AND co.condition_start_date >= op.observation_period_start_date
    AND co.condition_start_date <= op.observation_period_end_date
  WHERE co.condition_concept_id IN (SELECT concept_id FROM ami_concepts)
    AND co.condition_start_date >= DATEADD(day, -30, sp.first_index_date)
)
SELECT CAST(a.person_id AS VARCHAR(50)) AS person_id,
  a.condition_start_date,
  CASE WHEN EXISTS (
    SELECT 1 FROM @cdm_database_schema.visit_occurrence v
    WHERE v.person_id = a.person_id
      AND v.visit_concept_id IN (SELECT concept_id FROM visit_concepts)
      AND v.visit_start_date >= a.op_start_date
      AND v.visit_start_date <= a.condition_start_date
      AND v.visit_end_date >= a.condition_start_date
  ) THEN 1 ELSE 0 END AS visit_spanning,
  CASE WHEN EXISTS (
    SELECT 1 FROM @cdm_database_schema.visit_occurrence v
    WHERE v.person_id = a.person_id
      AND v.visit_concept_id IN (SELECT concept_id FROM visit_concepts)
      AND v.visit_start_date = a.condition_start_date
  ) THEN 1 ELSE 0 END AS visit_starting,
  CASE WHEN EXISTS (
    SELECT 1 FROM @cdm_database_schema.procedure_occurrence po
    WHERE po.person_id = a.person_id
      AND po.procedure_concept_id IN (SELECT concept_id FROM pci_concepts)
      AND po.procedure_date >= DATEADD(day, -6, a.condition_start_date)
      AND po.procedure_date <= DATEADD(day, -1, a.condition_start_date)
      AND po.procedure_date >= a.op_start_date
  ) THEN 1 ELSE 0 END AS pci_prior_1_6
FROM ami a;"

cohortEntriesSql <- "
SELECT CAST(o.subject_id AS VARCHAR(50)) AS person_id,
  o.cohort_definition_id,
  o.cohort_start_date,
  o.cohort_end_date
FROM @cohort_database_schema.@cohort_table o
WHERE o.cohort_definition_id IN (@outcome_ids)
  AND o.subject_id IN (
    SELECT subject_id FROM @cohort_database_schema.@cohort_table
    WHERE cohort_definition_id IN (@target_id, @comparator_id)
  );"

abciximabSql <- "
SELECT c.cohort_definition_id,
  CASE WHEN de.drug_exposure_start_date >= DATEADD(day, -7, c.cohort_start_date)
       THEN 'day -7 to 0' ELSE 'day -365 to -8' END AS time_window,
  de.drug_concept_id,
  de.drug_source_concept_id,
  de.drug_source_value,
  COUNT(DISTINCT c.subject_id) AS persons,
  COUNT(*) AS records
FROM @cohort_database_schema.@cohort_table c
INNER JOIN @cdm_database_schema.drug_exposure de
  ON de.person_id = c.subject_id
WHERE c.cohort_definition_id IN (@target_id, @comparator_id)
  AND de.drug_concept_id IN (
    SELECT descendant_concept_id FROM @cdm_database_schema.concept_ancestor WHERE ancestor_concept_id = 19047423
  )
  AND de.drug_exposure_start_date >= DATEADD(day, -365, c.cohort_start_date)
  AND de.drug_exposure_start_date <= c.cohort_start_date
GROUP BY c.cohort_definition_id,
  CASE WHEN de.drug_exposure_start_date >= DATEADD(day, -7, c.cohort_start_date)
       THEN 'day -7 to 0' ELSE 'day -365 to -8' END,
  de.drug_concept_id,
  de.drug_source_concept_id,
  de.drug_source_value;"

runHiraQueries <- function(connectionDetails, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable,
                           targetId, comparatorId) {
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  query <- function(sql, ...) {
    DatabaseConnector::renderTranslateQuerySql(
      connection, sql, snakeCaseToCamelCase = TRUE,
      cdm_database_schema = cdmDatabaseSchema, cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable, target_id = targetId, comparator_id = comparatorId, ...
    )
  }
  message("Query 1/3: AMI records (read-only)")
  amiRecords <- query(amiRecordsSql)
  message("Query 2/3: existing outcome cohort entries (read-only)")
  cohortEntries <- query(cohortEntriesSql, outcome_ids = c(deathCohortId, amiCohortId, 1792372, 1792373, 1792374,
                                                            1792366, 1792367, 1792369))
  message("Query 3/3: abciximab records (read-only, aggregate)")
  abciximab <- query(abciximabSql)
  list(amiRecords = amiRecords, cohortEntries = cohortEntries, abciximab = abciximab)
}

# Outcome eras -------------------------------------------------------------------------
# Dates to days relative to each cohort row's index date
toStudyDays <- function(records, cohorts, dateColumn, endColumn = NULL) {
  records$personId <- idToChar(records$personId)
  records$startDate <- as.Date(records[[dateColumn]])
  records$endDate <- if (is.null(endColumn)) records$startDate + 1 else as.Date(records[[endColumn]])
  records %>%
    select(-any_of(c("rowId", "cohortStartDate", "cohortEndDate", "conditionStartDate"))) %>%
    inner_join(cohorts %>% select("rowId", "personId", indexDate = "cohortStartDate"), by = "personId") %>%
    mutate(start = as.numeric(.data$startDate - .data$indexDate), end = as.numeric(.data$endDate - .data$indexDate))
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

# Era starts (days from index) per rowId for the parent outcomes rebuilt from
# components ("rebuilt"), and for the V1/V2 variants
buildOutcomeEras <- function(queryResults, cohorts) {
  ami <- toStudyDays(queryResults$amiRecords, cohorts, "conditionStartDate")
  entries <- queryResults$cohortEntries
  componentIntervals <- function(cohortId) {
    e <- entries %>% filter(.data$cohortDefinitionId == cohortId)
    if (cohortId == deathCohortId) {
      toStudyDays(e, cohorts, "cohortStartDate") # composite end strategy: start + 1
    } else {
      toStudyDays(e, cohorts, "cohortStartDate", "cohortEndDate")
    }
  }
  amiIntervals <- list(
    original = ami %>% filter(.data$visitSpanning == 1),
    V1 = ami %>% filter(.data$visitStarting == 1),
    V2 = ami %>% filter(.data$visitSpanning == 1, .data$pciPrior16 == 0)
  )
  build <- function(parentId, amiVersion) {
    parts <- lapply(outcomeComponents[[as.character(parentId)]], function(component) {
      if (component == "ami") amiIntervals[[amiVersion]] else if (component == "death")
        componentIntervals(deathCohortId) else componentIntervals(as.numeric(component))
    })
    collapseEras(bind_rows(lapply(parts, function(p) p %>% select("rowId", "start", "end"))))
  }
  eras <- list()
  for (parentId in names(outcomeComponents)) {
    eras[[paste0("rebuilt_", parentId)]] <- build(parentId, "original")
    for (variant in c("V1", "V2")) {
      outcomeId <- newOutcomes$outcomeId[newOutcomes$parentId == as.numeric(parentId) & newOutcomes$variant == variant]
      eras[[as.character(outcomeId)]] <- build(parentId, variant)
    }
  }
  # As stored in the cohort table (for validation)
  for (cohortId in c(amiCohortId, 1792366, 1792367, 1792369)) {
    eras[[paste0("cohortTable_", cohortId)]] <- collapseEras(
      componentIntervals(cohortId) %>% select("rowId", "start", "end")
    )
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

# Main ------------------------------------------------------------------------------
runHiraRevision <- function(outputFolder,
                            exportFolder,
                            databaseId,
                            cmAnalysisList,
                            targetId = 1792797,
                            comparatorId = 1793457,
                            connectionDetails = NULL,
                            cdmDatabaseSchema = NULL,
                            cohortDatabaseSchema = NULL,
                            cohortTable = NULL,
                            queryResults = NULL, # testing only: pre-computed query results
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

  if (is.null(queryResults)) {
    queryResults <- runHiraQueries(connectionDetails, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable,
                                   targetId, comparatorId)
  }
  cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, reference$cohortMethodDataFile[1]))
  cohorts <- cmData$cohorts %>%
    select("rowId", "personId", "treatment", "cohortStartDate") %>%
    collect() %>%
    mutate(personId = idToChar(.data$personId))
  denominators <- cohorts %>% group_by(.data$treatment) %>% summarise(cohortSubjects = n(), .groups = "drop")
  Andromeda::close(cmData)

  message("Building outcome eras")
  eras <- buildOutcomeEras(queryResults, cohorts)

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
      # Validation: parent outcome rebuilt from components must reproduce the saved estimate
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

  message("Abciximab records")
  abciximab <- queryResults$abciximab %>%
    mutate(treatment = if_else(.data$cohortDefinitionId == targetId, 1, 0)) %>%
    left_join(denominators, by = "treatment") %>%
    mutate(across(c("persons", "records"), ~ censorCounts(.x, minCellCount)))
  write(abciximab, "abciximab_records")

  list(primaryPopulations = primaryPopulations, validation = validation)
}
