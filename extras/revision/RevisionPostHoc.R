# JACC Asia revision: functions for the post hoc analyses (sourced automatically; do not edit).
library(dplyr)

# Definitions -------------------------------------------------------------------------
revisionAnalysisId <- 999

# ACS type, from ConditionGroupEraShortTerm covariates (concept incl. descendants)
acsConcepts <- list(
  stemi = c(4296653, 45766114), # acute / subsequent STEMI
  nstemi = c(4270024, 45766241, 444406, 765132), # acute / subsequent NSTEMI, subendocardial
  wallSite = c(434376, 438170, 438438, 438447, 441579, 436706, 439693), # AMI of a named wall
  ami = c(312327, 4108217), # acute MI, subsequent MI
  unstableAngina = c(315296) # preinfarction syndrome
)

# Severity markers: matched on concept names of short-term covariates (any domain)
severityMarkers <- list(
  cardiogenicShock = list(
    include = "cardiogenic shock",
    exclude = "history of"
  ),
  cardiacArrest = list(
    include = "cardiac arrest|ventricular fibrillation|cardiopulmonary resuscitation",
    exclude = "history of|family history"
  ),
  mechanicalCirculatorySupport = list(
    include = "balloon pump|intra-?aortic balloon|counterpulsation|ventricular assist|heart assist|extracorporeal membrane oxygenat|ecmo|extracorporeal life support|impella",
    exclude = "external counterpulsation|history of"
  ),
  mechanicalVentilation = list(
    include = "mechanical ventilation|ventilation assist|ventilator|endotracheal|intubation of trachea",
    exclude = "non-?invasive|home ventilator|hyperventilation|lacrimal|anesthesia|anaesthesia|scan|speech valve|dependence on|history of"
  )
)

# Covariate IDs of the derived variables (analysisId 999)
revisionCovariates <- data.frame(
  covariateId = c(
    9990011, 9990012, 9990013, 9990014,
    9990021, 9990022, 9990023,
    9990031, 9990032, 9990033,
    9990041, 9990042, 9990043,
    9990051, 9990052, 9990053, 9990054, 9990055
  ),
  covariateName = c(
    "Subgroup: female", "Subgroup: age >= 75", "Subgroup: diabetes (365 d)", "Subgroup: chronic kidney disease (365 d)",
    "ACS type A: acute MI", "ACS type A: unstable angina without MI", "ACS type A: neither",
    "ACS type B (strict): STEMI", "ACS type B (strict): NSTE-ACS", "ACS type B (strict): unclassified",
    "ACS type C (broad): STEMI incl. wall-site AMI", "ACS type C (broad): NSTE-ACS", "ACS type C (broad): unclassified",
    "Severity: cardiogenic shock", "Severity: cardiac arrest", "Severity: mechanical circulatory support",
    "Severity: mechanical ventilation", "Severity: any of the four"
  )
)

# Subgroup levels: positive ID = has the derived covariate, negative ID = does not
subgroupVariables <- list(
  sex = c(female = 9990011, male = -9990011),
  age = c(age75plus = 9990012, ageUnder75 = -9990012),
  diabetes = c(diabetes = 9990013, noDiabetes = -9990013),
  ckd = c(ckd = 9990014, noCkd = -9990014),
  acsTypeA = c(ami = 9990021, unstableAngina = 9990022, neither = 9990023),
  acsTypeB = c(stemi = 9990031, nsteAcs = 9990032, unclassified = 9990033),
  acsTypeC = c(stemi = 9990041, nsteAcs = 9990042, unclassified = 9990043)
)

# Helpers ---------------------------------------------------------------------------
censorCounts <- function(x, minCellCount) {
  ifelse(!is.na(x) & x > 0 & x < minCellCount, -minCellCount, x)
}

idToChar <- function(x) {
  if (is.character(x)) return(trimws(x))
  if (inherits(x, "integer64")) return(as.character(x))
  sprintf("%.0f", x)
}

loadRevisionInputs <- function(cmFolder, analysisId, targetId, comparatorId, outcomeId) {
  reference <- as.data.frame(readRDS(file.path(cmFolder, "outcomeModelReference.rds")))
  row <- reference[reference$analysisId == analysisId & reference$targetId == targetId &
                     reference$comparatorId == comparatorId & reference$outcomeId == outcomeId, ]
  if (nrow(row) != 1) stop(sprintf("No saved files for analysis %s, outcome %s", analysisId, outcomeId))
  populationFile <- if (row$strataFile != "") row$strataFile else row$studyPopFile
  negativeControlIds <- reference$outcomeId[reference$analysisId == analysisId & reference$targetId == targetId &
                                              reference$comparatorId == comparatorId & !reference$outcomeOfInterest]
  list(cmDataFile = file.path(cmFolder, row$cohortMethodDataFile),
       sharedPsFile = file.path(cmFolder, row$sharedPsFile),
       population = readRDS(file.path(cmFolder, populationFile)),
       negativeControlIds = negativeControlIds)
}

# Derived covariates ----------------------------------------------------------------
createRevisionCovariates <- function(cmData) {
  covariateRef <- cmData$covariateRef %>% collect()
  analysisRef <- cmData$analysisRef %>% collect()
  shortTermAnalyses <- analysisRef$analysisId[grepl("ShortTerm", analysisRef$analysisName)]
  groupShortTerm <- analysisRef$analysisId[analysisRef$analysisName == "ConditionGroupEraShortTerm"]

  rowsWith <- function(covariateIds) {
    if (length(covariateIds) == 0) return(numeric(0))
    cmData$covariates %>%
      filter(.data$covariateId %in% !!covariateIds) %>%
      distinct(.data$rowId) %>%
      pull(.data$rowId)
  }
  groupRows <- function(conceptIds) {
    rowsWith(covariateRef$covariateId[covariateRef$conceptId %in% conceptIds &
                                        covariateRef$analysisId %in% groupShortTerm])
  }
  allRows <- cmData$cohorts %>% pull(.data$rowId)

  # Subgroups
  ageGroups <- covariateRef %>%
    filter(.data$analysisId == 3) %>%
    mutate(lower = as.numeric(sub("^\\D*(\\d+)\\s*-.*$", "\\1", .data$covariateName, perl = TRUE)))
  rows <- list(
    "9990011" = rowsWith(8532001),
    "9990012" = rowsWith(ageGroups$covariateId[ageGroups$lower >= 75]),
    "9990013" = rowsWith(covariateRef$covariateId[covariateRef$conceptId == 201820 & covariateRef$analysisId == 210]),
    "9990014" = rowsWith(covariateRef$covariateId[covariateRef$conceptId == 46271022 & covariateRef$analysisId == 210])
  )

  # ACS type
  stemi <- groupRows(acsConcepts$stemi)
  wallSite <- groupRows(acsConcepts$wallSite)
  nste <- union(groupRows(acsConcepts$nstemi), groupRows(acsConcepts$unstableAngina))
  ami <- Reduce(union, list(groupRows(acsConcepts$ami), stemi, wallSite, groupRows(acsConcepts$nstemi)))
  unstableAngina <- setdiff(groupRows(acsConcepts$unstableAngina), ami)
  stemiBroad <- union(stemi, wallSite)
  rows[["9990021"]] <- ami
  rows[["9990022"]] <- unstableAngina
  rows[["9990023"]] <- setdiff(allRows, union(ami, unstableAngina))
  rows[["9990031"]] <- stemi
  rows[["9990032"]] <- setdiff(nste, stemi)
  rows[["9990033"]] <- setdiff(allRows, union(stemi, nste))
  rows[["9990041"]] <- stemiBroad
  rows[["9990042"]] <- setdiff(nste, stemiBroad)
  rows[["9990043"]] <- setdiff(allRows, union(stemiBroad, nste))

  # Severity markers
  candidates <- covariateRef %>%
    filter(.data$analysisId %in% shortTermAnalyses) %>%
    mutate(conceptName = tolower(sub(".*relative to index: ", "", .data$covariateName)))
  conceptMatches <- list()
  markerIds <- c(cardiogenicShock = "9990051", cardiacArrest = "9990052",
                 mechanicalCirculatorySupport = "9990053", mechanicalVentilation = "9990054")
  for (marker in names(severityMarkers)) {
    def <- severityMarkers[[marker]]
    matched <- candidates %>%
      filter(grepl(def$include, .data$conceptName), !grepl(def$exclude, .data$conceptName))
    rows[[markerIds[[marker]]]] <- rowsWith(matched$covariateId)
    if (nrow(matched) > 0) {
      conceptMatches[[marker]] <- matched %>%
        transmute(marker = !!marker, .data$covariateId, .data$covariateName, .data$analysisId)
    }
  }
  rows[["9990055"]] <- Reduce(union, rows[markerIds])

  covariates <- bind_rows(lapply(names(rows), function(id) {
    if (length(rows[[id]]) == 0) return(NULL)
    data.frame(rowId = rows[[id]], covariateId = as.numeric(id), covariateValue = 1)
  }))
  list(covariates = covariates, conceptMatches = bind_rows(conceptMatches))
}

# A small CohortMethodData holding only the covariates needed, so that each CohortMethod
# version computes balance with its own weighting without processing all covariates.
createSmallCmData <- function(cmData, revisionCovariateData, extraCovariateIds) {
  covariateRef <- cmData$covariateRef %>% collect()
  analysisRef <- cmData$analysisRef %>% collect()
  extraRef <- covariateRef %>% filter(.data$covariateId %in% extraCovariateIds)
  newRef <- revisionCovariates %>%
    mutate(analysisId = revisionAnalysisId, conceptId = 0)
  for (col in setdiff(colnames(covariateRef), colnames(newRef))) newRef[[col]] <- NA
  newAnalysisRef <- analysisRef[1, ]
  newAnalysisRef$analysisId <- revisionAnalysisId
  newAnalysisRef$analysisName <- "RevisionDerived"
  newAnalysisRef$domainId <- "Revision"
  newAnalysisRef$isBinary <- "Y"
  newAnalysisRef$missingMeansZero <- "Y"
  extraCovariates <- cmData$covariates %>%
    filter(.data$covariateId %in% !!extraCovariateIds) %>%
    collect()
  derived <- revisionCovariateData$covariates
  derived$rowId <- methods::as(derived$rowId, class(extraCovariates$rowId)[1])
  derived$covariateId <- as.numeric(derived$covariateId)
  derived$covariateValue <- methods::as(derived$covariateValue, class(extraCovariates$covariateValue)[1])
  covariates <- bind_rows(extraCovariates %>% mutate(covariateId = as.numeric(.data$covariateId)), derived)
  small <- Andromeda::andromeda(
    cohorts = cmData$cohorts %>% collect(),
    outcomes = cmData$outcomes %>% filter(.data$rowId < 0) %>% collect(),
    covariates = covariates,
    covariateRef = bind_rows(extraRef, newRef[, colnames(covariateRef)]),
    analysisRef = bind_rows(analysisRef %>% filter(.data$analysisId %in% extraRef$analysisId), newAnalysisRef)
  )
  attr(small, "metaData") <- attr(cmData, "metaData")
  class(small) <- "CohortMethodData"
  attr(class(small), "package") <- "CohortMethod"
  small
}

computeBalance <- function(population, smallCmData, subgroupCovariateId = NULL, minCellCount) {
  balance <- CohortMethod::computeCovariateBalance(
    population = population,
    cohortMethodData = smallCmData,
    subgroupCovariateId = subgroupCovariateId
  )
  # A covariate absent in one arm comes back as NA; it is 0 (all derived covariates are binary)
  binaryStdDiff <- function(t, c) {
    sd <- sqrt(pmax(0, (t * (1 - t) + c * (1 - c)) / 2))
    if_else(sd == 0, 0, (t - c) / sd)
  }
  balance %>%
    mutate(
      across(c("beforeMatchingSumTarget", "beforeMatchingSumComparator", "beforeMatchingMeanTarget",
               "beforeMatchingMeanComparator", "afterMatchingMeanTarget", "afterMatchingMeanComparator"),
             ~ coalesce(.x, 0)),
      beforeMatchingStdDiff = coalesce(.data$beforeMatchingStdDiff,
                                       binaryStdDiff(.data$beforeMatchingMeanTarget, .data$beforeMatchingMeanComparator)),
      afterMatchingStdDiff = coalesce(.data$afterMatchingStdDiff,
                                      binaryStdDiff(.data$afterMatchingMeanTarget, .data$afterMatchingMeanComparator))
    ) %>%
    transmute(
      .data$covariateId, .data$covariateName, .data$analysisId,
      beforeSumTarget = censorCounts(.data$beforeMatchingSumTarget, minCellCount),
      beforeSumComparator = censorCounts(.data$beforeMatchingSumComparator, minCellCount),
      beforeMeanTarget = if_else(beforeSumTarget < 0, NA_real_, .data$beforeMatchingMeanTarget),
      beforeMeanComparator = if_else(beforeSumComparator < 0, NA_real_, .data$beforeMatchingMeanComparator),
      beforeStdDiff = .data$beforeMatchingStdDiff,
      afterMeanTarget = if_else(beforeSumTarget < 0, NA_real_, .data$afterMatchingMeanTarget),
      afterMeanComparator = if_else(beforeSumComparator < 0, NA_real_, .data$afterMatchingMeanComparator),
      afterStdDiff = .data$afterMatchingStdDiff
    ) %>%
    arrange(.data$covariateId)
}

# Subgroup analyses -----------------------------------------------------------------
subgroupRows <- function(levelId, revisionCovariateData, population) {
  inLevel <- revisionCovariateData$covariates$rowId[revisionCovariateData$covariates$covariateId == abs(levelId)]
  if (levelId > 0) population$rowId[population$rowId %in% inLevel] else population$rowId[!population$rowId %in% inLevel]
}

fitStratifiedCox <- function(population) {
  # No estimate unless both arms have at least one outcome (otherwise unbounded)
  if (sum(population$outcomeCount[population$treatment == 1] > 0) == 0 ||
      sum(population$outcomeCount[population$treatment == 0] > 0) == 0) {
    return(data.frame(logRr = NA_real_, logLb95 = NA_real_, logUb95 = NA_real_, seLogRr = NA_real_))
  }
  estimate <- tryCatch({
    model <- CohortMethod::fitOutcomeModel(population = population, modelType = "cox", stratified = TRUE)
    model$outcomeModelTreatmentEstimate
  }, error = function(e) NULL)
  if (is.null(estimate) || nrow(estimate) == 0 || !is.finite(estimate$seLogRr) || !is.finite(estimate$logUb95)) {
    return(data.frame(logRr = NA_real_, logLb95 = NA_real_, logUb95 = NA_real_, seLogRr = NA_real_))
  }
  data.frame(logRr = estimate$logRr, logLb95 = estimate$logLb95, logUb95 = estimate$logUb95, seLogRr = estimate$seLogRr)
}

# Negative-control populations are not saved by CohortMethod. They are rebuilt here as
# CohortMethod built them (study population -> shared PS -> PS strata); this reproduces
# the saved populations and negative-control estimates exactly.
rebuildStratifiedPopulation <- function(cmData, sharedPs, createStudyPopArgs, stratifyByPsArgs, outcomeId) {
  args <- createStudyPopArgs[names(createStudyPopArgs) %in% names(formals(CohortMethod::createStudyPopulation))]
  population <- do.call(CohortMethod::createStudyPopulation,
                        c(list(cohortMethodData = cmData, outcomeId = outcomeId), args))
  psColumns <- intersect(c("rowId", "propensityScore", "preferenceScore"), colnames(sharedPs))
  population <- population %>% inner_join(sharedPs[, psColumns], by = "rowId")
  args <- stratifyByPsArgs[names(stratifyByPsArgs) %in% names(formals(CohortMethod::stratifyByPs))]
  do.call(CohortMethod::stratifyByPs, c(list(population = population), args))
}

# Negative-control estimates in the whole population ("all") and in every subgroup level
subgroupNegativeControlEstimates <- function(cmData, sharedPs, createStudyPopArgs, stratifyByPsArgs,
                                             negativeControlIds, revisionCovariateData) {
  present <- cmData$outcomes %>% distinct(.data$outcomeId) %>% pull(.data$outcomeId)
  levels <- data.frame(subgroup = "all", level = "all", levelId = NA_real_)
  for (variable in names(subgroupVariables)) {
    levels <- rbind(levels, data.frame(subgroup = variable, level = names(subgroupVariables[[variable]]),
                                       levelId = unname(subgroupVariables[[variable]])))
  }
  estimates <- list()
  for (outcomeId in intersect(negativeControlIds, present)) {
    population <- suppressMessages(suppressWarnings(
      rebuildStratifiedPopulation(cmData, sharedPs, createStudyPopArgs, stratifyByPsArgs, outcomeId)
    ))
    for (i in seq_len(nrow(levels))) {
      sub <- if (is.na(levels$levelId[i])) population else
        population[population$rowId %in% subgroupRows(levels$levelId[i], revisionCovariateData, population), ]
      est <- suppressMessages(suppressWarnings(fitStratifiedCox(sub)))
      estimates[[length(estimates) + 1]] <- data.frame(outcomeId = outcomeId, subgroup = levels$subgroup[i],
                                                       level = levels$level[i], logRr = est$logRr, seLogRr = est$seLogRr)
    }
  }
  bind_rows(estimates) %>% filter(is.finite(.data$logRr), is.finite(.data$seLogRr))
}

# Calibrate with the level's own negative controls when at least minNegativeControls have
# estimates; otherwise with the whole-population negative controls.
calibrateSubgroupResults <- function(results, negativeControlEstimates, minNegativeControls) {
  overall <- negativeControlEstimates %>% filter(.data$subgroup == "all")
  calibrated <- lapply(seq_len(nrow(results)), function(i) {
    r <- results[i, ]
    own <- negativeControlEstimates %>% filter(.data$subgroup == r$subgroup, .data$level == r$level)
    if (nrow(own) >= minNegativeControls) {
      nc <- own; method <- "subgroup negative controls"
    } else if (nrow(overall) >= minNegativeControls) {
      nc <- overall; method <- "whole-population negative controls"
    } else {
      nc <- NULL; method <- "not calibrated (too few negative controls)"
    }
    out <- data.frame(calibrationMethod = method, negativeControlsUsed = if (is.null(nc)) 0 else nrow(nc),
                      calibratedLogRr = NA_real_, calibratedSeLogRr = NA_real_,
                      calibratedLogLb95 = NA_real_, calibratedLogUb95 = NA_real_, calibratedP = NA_real_)
    if (!is.null(nc) && is.finite(r$seLogRr)) {
      tryCatch({
        model <- EmpiricalCalibration::fitSystematicErrorModel(nc$logRr, nc$seLogRr, rep(0, nrow(nc)))
        ci <- EmpiricalCalibration::calibrateConfidenceInterval(r$logRr, r$seLogRr, model)
        null <- EmpiricalCalibration::fitNull(nc$logRr, nc$seLogRr)
        out$calibratedLogRr <- ci$logRr
        out$calibratedSeLogRr <- ci$seLogRr
        out$calibratedLogLb95 <- ci$logLb95Rr
        out$calibratedLogUb95 <- ci$logUb95Rr
        out$calibratedP <- EmpiricalCalibration::calibrateP(null, r$logRr, r$seLogRr)
      }, error = function(e) out$calibrationMethod <<- paste("calibration failed:", conditionMessage(e)))
    }
    out
  })
  bind_cols(results, bind_rows(calibrated))
}

# Heterogeneity across levels of each subgroup variable (Cochran's Q on log HRs);
# with 2 levels this is the z-test for the difference
heterogeneityP <- function(results, logRrColumn, seColumn) {
  results %>%
    filter(.data$subgroup != "all", is.finite(.data[[seColumn]]), .data[[seColumn]] > 0) %>%
    group_by(.data$subgroup) %>%
    summarise(
      levelsWithEstimate = n(),
      q = {
        w <- 1 / .data[[seColumn]]^2
        sum(w * (.data[[logRrColumn]] - sum(w * .data[[logRrColumn]]) / sum(w))^2)
      },
      .groups = "drop"
    ) %>%
    transmute(.data$subgroup, p = if_else(.data$levelsWithEstimate > 1,
                                          pchisq(.data$q, df = .data$levelsWithEstimate - 1, lower.tail = FALSE),
                                          NA_real_))
}

runSubgroupAnalyses <- function(population, revisionCovariateData, minCellCount,
                                negativeControlEstimates = NULL, minNegativeControls = 20) {
  results <- list()
  for (variable in c("all", names(subgroupVariables))) {
    if (variable == "all") {
      sub <- population
      results[[1]] <- data.frame(
        subgroup = "all", level = "all",
        targetSubjects = sum(sub$treatment == 1), comparatorSubjects = sum(sub$treatment == 0),
        targetOutcomes = sum(sub$outcomeCount[sub$treatment == 1] > 0),
        comparatorOutcomes = sum(sub$outcomeCount[sub$treatment == 0] > 0),
        targetDays = sum(sub$timeAtRisk[sub$treatment == 1]),
        comparatorDays = sum(sub$timeAtRisk[sub$treatment == 0]),
        fitStratifiedCox(sub)
      )
      next
    }
    levels <- subgroupVariables[[variable]]
    for (level in names(levels)) {
      sub <- population[population$rowId %in% subgroupRows(levels[[level]], revisionCovariateData, population), ]
      est <- fitStratifiedCox(sub)
      results[[length(results) + 1]] <- data.frame(
        subgroup = variable, level = level,
        targetSubjects = sum(sub$treatment == 1), comparatorSubjects = sum(sub$treatment == 0),
        targetOutcomes = sum(sub$outcomeCount[sub$treatment == 1] > 0),
        comparatorOutcomes = sum(sub$outcomeCount[sub$treatment == 0] > 0),
        targetDays = sum(sub$timeAtRisk[sub$treatment == 1]),
        comparatorDays = sum(sub$timeAtRisk[sub$treatment == 0]),
        est
      )
    }
  }
  results <- bind_rows(results)
  if (!is.null(negativeControlEstimates)) {
    results <- calibrateSubgroupResults(results, negativeControlEstimates, minNegativeControls)
  }
  results <- results %>%
    left_join(heterogeneityP(results, "logRr", "seLogRr") %>% rename(interactionP = "p"), by = "subgroup")
  if (!is.null(negativeControlEstimates)) {
    results <- results %>%
      left_join(heterogeneityP(results, "calibratedLogRr", "calibratedSeLogRr") %>% rename(calibratedInteractionP = "p"),
                by = "subgroup") %>%
      mutate(calibratedHr = exp(.data$calibratedLogRr), calibratedCi95Lb = exp(.data$calibratedLogLb95),
             calibratedCi95Ub = exp(.data$calibratedLogUb95))
  }
  results %>%
    mutate(
      hr = exp(.data$logRr), ci95Lb = exp(.data$logLb95), ci95Ub = exp(.data$logUb95),
      across(c("targetSubjects", "comparatorSubjects", "targetOutcomes", "comparatorOutcomes"), ~ censorCounts(.x, minCellCount))
    )
}

subgroupBalanceSummary <- function(population, smallCmData, minCellCount) {
  bind_rows(lapply(names(subgroupVariables), function(variable) {
    levels <- subgroupVariables[[variable]]
    bind_rows(lapply(names(levels)[levels[names(levels)] > 0], function(level) {
      balance <- tryCatch(
        computeBalance(population, smallCmData, subgroupCovariateId = levels[[level]], minCellCount = minCellCount),
        error = function(e) NULL
      )
      if (is.null(balance)) return(NULL)
      balance %>% mutate(subgroup = variable, level = level, .before = 1)
    }))
  }))
}

# Zero time-at-risk exclusions --------------------------------------------------------
zeroTimeAtRiskReasons <- function(cmData, createStudyPopArgs, deathOutcomeId, minCellCount) {
  args <- createStudyPopArgs
  args <- args[setdiff(names(args), c("attr_class", "minDaysAtRisk"))]
  args <- args[names(args) %in% names(formals(CohortMethod::createStudyPopulation))]
  population <- do.call(CohortMethod::createStudyPopulation,
                        c(list(cohortMethodData = cmData, outcomeId = deathOutcomeId, minDaysAtRisk = 0), args))
  death <- cmData$outcomes %>%
    filter(.data$outcomeId == !!deathOutcomeId) %>%
    group_by(.data$rowId) %>%
    summarise(deathDay = min(.data$daysToEvent, na.rm = TRUE), .groups = "drop") %>%
    collect()
  population %>%
    mutate(zeroTimeAtRisk = .data$timeAtRisk < 1) %>%
    left_join(death, by = "rowId") %>%
    mutate(reason = case_when(
      !.data$zeroTimeAtRisk ~ "included (time at risk >= 1 day)",
      !is.na(.data$deathDay) & .data$deathDay < .data$riskStart ~ "death before start of time at risk",
      .data$daysToObsEnd < .data$riskStart ~ "observation ended before start of time at risk",
      .data$daysToCohortEnd < .data$riskStart ~ "exposure ended before start of time at risk",
      TRUE ~ "other"
    )) %>%
    group_by(.data$treatment, .data$reason) %>%
    summarise(subjects = n(), .groups = "drop") %>%
    mutate(subjects = censorCounts(.data$subjects, minCellCount))
}

# Study period ----------------------------------------------------------------------
studyPeriod <- function(population, minCellCount) {
  byYear <- population %>%
    mutate(indexYear = as.integer(format(.data$cohortStartDate, "%Y"))) %>%
    group_by(.data$treatment, .data$indexYear) %>%
    summarise(subjects = n(), .groups = "drop") %>%
    mutate(subjects = censorCounts(.data$subjects, minCellCount))
  range <- population %>%
    group_by(.data$treatment) %>%
    summarise(firstIndexDate = min(.data$cohortStartDate), lastIndexDate = max(.data$cohortStartDate), .groups = "drop")
  list(byYear = byYear, range = range)
}

# Switching and discontinuation -----------------------------------------------------
p2y12Ingredients <- c(clopidogrel = 1322184, ticagrelor = 40241186, prasugrel = 40163718)

getP2y12Records <- function(connectionDetails, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable,
                            targetId, comparatorId) {
  sql <- "
SELECT CAST(c.subject_id AS VARCHAR(50)) AS person_id,
  c.cohort_definition_id,
  c.cohort_start_date,
  ca.ancestor_concept_id AS ingredient_concept_id,
  de.drug_exposure_start_date,
  COALESCE(de.drug_exposure_end_date,
           DATEADD(day, CASE WHEN de.days_supply > 0 THEN de.days_supply - 1 ELSE 0 END, de.drug_exposure_start_date)) AS drug_exposure_end_date
FROM @cohort_database_schema.@cohort_table c
INNER JOIN @cdm_database_schema.drug_exposure de
  ON de.person_id = c.subject_id
INNER JOIN @cdm_database_schema.concept_ancestor ca
  ON ca.descendant_concept_id = de.drug_concept_id
WHERE c.cohort_definition_id IN (@target_id, @comparator_id)
  AND ca.ancestor_concept_id IN (@ingredient_ids)
  AND de.drug_exposure_start_date >= DATEADD(day, -30, c.cohort_start_date)
  AND de.drug_exposure_start_date <= DATEADD(day, 365, c.cohort_start_date);"
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  DatabaseConnector::renderTranslateQuerySql(
    connection, sql, snakeCaseToCamelCase = TRUE,
    cdm_database_schema = cdmDatabaseSchema, cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable, target_id = targetId, comparator_id = comparatorId,
    ingredient_ids = p2y12Ingredients
  )
}

# records: personId, cohortDefinitionId, cohortStartDate, ingredientConceptId,
# drugExposureStartDate, drugExposureEndDate (from getP2y12Records)
switchingProfile <- function(population, records, targetId, comparatorId, targetIngredient, comparatorIngredient,
                             horizons = c(30, 90, 180, 365), gapDays = 14, minCellCount) {
  records <- records %>%
    mutate(
      personId = idToChar(.data$personId),
      treatment = if_else(.data$cohortDefinitionId == targetId, 1, 0),
      cohortStartDate = as.Date(.data$cohortStartDate),
      start = as.numeric(as.Date(.data$drugExposureStartDate) - .data$cohortStartDate),
      end = pmax(.data$start, as.numeric(as.Date(.data$drugExposureEndDate) - .data$cohortStartDate))
    )
  people <- population %>%
    transmute(.data$rowId, personId = idToChar(.data$personId), .data$treatment, .data$cohortStartDate,
              followUpEnd = pmin(365, .data$daysToObsEnd))
  # Merge exposures of the same ingredient into eras allowing gaps of up to gapDays
  eras <- records %>%
    inner_join(people, by = c("personId", "treatment", "cohortStartDate")) %>%
    arrange(.data$rowId, .data$ingredientConceptId, .data$start) %>%
    group_by(.data$rowId, .data$ingredientConceptId) %>%
    mutate(newEra = cumsum(c(1, (.data$start[-1] > cummax(.data$end)[-n()] + gapDays)))) %>%
    group_by(.data$rowId, .data$ingredientConceptId, .data$newEra) %>%
    summarise(eraStart = min(.data$start), eraEnd = max(.data$end), .groups = "drop")
  indexIngredient <- function(treatment) if_else(treatment == 1, targetIngredient, comparatorIngredient)
  otherStudyIngredient <- function(treatment) if_else(treatment == 1, comparatorIngredient, targetIngredient)

  person <- people %>%
    left_join(
      eras %>%
        inner_join(people %>% select("rowId", "treatment"), by = "rowId") %>%
        filter(.data$ingredientConceptId == indexIngredient(.data$treatment), .data$eraStart <= 1) %>%
        group_by(.data$rowId) %>%
        summarise(indexEraEnd = max(.data$eraEnd), .groups = "drop"),
      by = "rowId"
    ) %>%
    mutate(indexEraEnd = coalesce(.data$indexEraEnd, 0)) %>%
    left_join(
      eras %>%
        inner_join(people %>% select("rowId", "treatment"), by = "rowId") %>%
        filter(.data$ingredientConceptId != indexIngredient(.data$treatment), .data$eraStart >= 1) %>%
        group_by(.data$rowId) %>%
        slice_min(.data$eraStart, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        transmute(.data$rowId, switchDay = .data$eraStart,
                  switchTo = if_else(.data$ingredientConceptId == otherStudyIngredient(.data$treatment),
                                     "other study drug", "clopidogrel")),
      by = "rowId"
    ) %>%
    mutate(
      discontinuationDay = .data$indexEraEnd + 1,
      firstChange = case_when(
        !is.na(.data$switchDay) & .data$switchDay <= .data$indexEraEnd + gapDays & .data$switchDay <= .data$followUpEnd ~
          paste("switched to", .data$switchTo),
        .data$discontinuationDay + gapDays <= .data$followUpEnd ~ "discontinued all P2Y12",
        TRUE ~ "no change during follow-up"
      ),
      firstChangeDay = case_when(
        startsWith(.data$firstChange, "switched") ~ .data$switchDay,
        .data$firstChange == "discontinued all P2Y12" ~ .data$discontinuationDay,
        TRUE ~ NA_real_
      )
    )

  firstChange <- person %>%
    group_by(.data$treatment, .data$firstChange) %>%
    summarise(
      subjects = n(),
      p25Day = if (n() >= minCellCount) quantile(.data$firstChangeDay, 0.25, na.rm = TRUE) else NA_real_,
      medianDay = if (n() >= minCellCount) median(.data$firstChangeDay, na.rm = TRUE) else NA_real_,
      p75Day = if (n() >= minCellCount) quantile(.data$firstChangeDay, 0.75, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(subjects = censorCounts(.data$subjects, minCellCount))

  statusAt <- bind_rows(lapply(horizons, function(t) {
    onDrug <- eras %>%
      filter(.data$eraStart <= t, .data$eraEnd >= t) %>%
      inner_join(people %>% select("rowId", "treatment"), by = "rowId") %>%
      mutate(state = case_when(
        .data$ingredientConceptId == indexIngredient(.data$treatment) ~ 1,
        .data$ingredientConceptId == otherStudyIngredient(.data$treatment) ~ 2,
        TRUE ~ 3
      )) %>%
      group_by(.data$rowId) %>%
      summarise(state = min(.data$state), .groups = "drop")
    person %>%
      select("rowId", "treatment", "followUpEnd") %>%
      left_join(onDrug, by = "rowId") %>%
      mutate(status = case_when(
        .data$followUpEnd < t ~ "follow-up ended (death or end of observation)",
        .data$state == 1 ~ "on index drug",
        .data$state == 2 ~ "on other study drug",
        .data$state == 3 ~ "on clopidogrel",
        TRUE ~ "no P2Y12 inhibitor"
      )) %>%
      group_by(.data$treatment, .data$status) %>%
      summarise(subjects = n(), .groups = "drop") %>%
      mutate(day = t, .before = 1)
  })) %>%
    mutate(subjects = censorCounts(.data$subjects, minCellCount))

  list(firstChange = firstChange, statusAt = statusAt)
}

# Main ------------------------------------------------------------------------------
# primaryAnalysisId / onTreatmentAnalysisId: 1-year ITT and on-treatment PS-stratification
#   analyses (HIRA: 8 and 2; Strategus: 8 and 5). createStudyPopArgs: theirs, named by ID.
# stratifyByPsArgs: given -> subgroup HRs calibrated with negative controls (the level's own
#   if >= minNegativeControls, else whole-population controls).
# populations: optional outcome ID -> population, for outcomes not in the saved files (HIRA).
# p2y12Records: records already read from the cohort DB (HIRA); otherwise queried with connectionDetails.
#   Neither given -> switching profile skipped (no database access).
runRevisionPostHoc <- function(cmFolder,
                               exportFolder,
                               databaseId,
                               targetId = 1792797,
                               comparatorId = 1793457,
                               primaryAnalysisId = 8,
                               onTreatmentAnalysisId,
                               createStudyPopArgs,
                               stratifyByPsArgs = NULL,
                               minNegativeControls = 20,
                               subgroupOutcomeIds = c(1792367, 17923671, 17923672),
                               populations = list(),
                               deathOutcomeId = 1792364,
                               p2y12Records = NULL,
                               connectionDetails = NULL,
                               cdmDatabaseSchema = NULL,
                               cohortDatabaseSchema = NULL,
                               cohortTable = NULL,
                               minCellCount = 5) {
  dir.create(exportFolder, showWarnings = FALSE, recursive = TRUE)
  write <- function(x, name) {
    if (is.null(x) || nrow(x) == 0) return(invisible())
    x <- x %>% mutate(databaseId = !!databaseId, .before = 1)
    utils::write.csv(x, file.path(exportFolder, paste0("revision_", name, ".csv")), row.names = FALSE)
  }
  message("Loading primary analysis files")
  primary <- loadRevisionInputs(cmFolder, primaryAnalysisId, targetId, comparatorId, subgroupOutcomeIds[1])
  cmData <- CohortMethod::loadCohortMethodData(primary$cmDataFile)
  on.exit(Andromeda::close(cmData), add = TRUE)
  population <- primary$population
  if (!"personId" %in% colnames(population)) {
    population <- population %>%
      inner_join(cmData$cohorts %>% select("rowId", "personId") %>% collect(), by = "rowId")
  }

  message("Deriving subgroup, ACS-type and severity variables")
  revisionCovariateData <- createRevisionCovariates(cmData)
  write(revisionCovariateData$conceptMatches, "severity_concepts")

  message("Balance of derived variables and age groups")
  ageAndSex <- cmData$covariateRef %>%
    filter(.data$analysisId %in% c(1, 3)) %>%
    pull(.data$covariateId)
  smallCmData <- createSmallCmData(cmData, revisionCovariateData, ageAndSex)
  on.exit(Andromeda::close(smallCmData), add = TRUE)
  write(computeBalance(population, smallCmData, minCellCount = minCellCount), "characteristics_balance")
  write(subgroupBalanceSummary(population, smallCmData, minCellCount), "subgroup_balance")

  negativeControlEstimates <- NULL
  if (!is.null(stratifyByPsArgs)) {
    message("Negative controls within subgroups (rebuilt from saved files; this takes a few minutes)")
    negativeControlEstimates <- subgroupNegativeControlEstimates(
      cmData = cmData,
      sharedPs = readRDS(primary$sharedPsFile),
      createStudyPopArgs = createStudyPopArgs[[as.character(primaryAnalysisId)]],
      stratifyByPsArgs = stratifyByPsArgs,
      negativeControlIds = primary$negativeControlIds,
      revisionCovariateData = revisionCovariateData
    )
    write(negativeControlEstimates, "subgroup_negative_controls")
  }

  message("Subgroup analyses")
  subgroupResults <- bind_rows(lapply(subgroupOutcomeIds, function(outcomeId) {
    pop <- populations[[as.character(outcomeId)]]
    if (is.null(pop)) {
      pop <- tryCatch(loadRevisionInputs(cmFolder, primaryAnalysisId, targetId, comparatorId, outcomeId)$population,
                      error = function(e) NULL)
    }
    if (is.null(pop)) {
      warning("No population for outcome ", outcomeId)
      return(NULL)
    }
    runSubgroupAnalyses(pop, revisionCovariateData, minCellCount, negativeControlEstimates, minNegativeControls) %>%
      mutate(outcomeId = !!outcomeId, .before = 1)
  }))
  write(subgroupResults, "subgroup_results")

  message("Zero time-at-risk exclusions")
  zeroTar <- bind_rows(lapply(c(primaryAnalysisId, onTreatmentAnalysisId), function(analysisId) {
    zeroTimeAtRiskReasons(cmData, createStudyPopArgs[[as.character(analysisId)]], deathOutcomeId, minCellCount) %>%
      mutate(analysisId = !!analysisId, .before = 1)
  }))
  write(zeroTar, "zero_time_at_risk")

  message("Study period")
  period <- studyPeriod(population, minCellCount)
  write(period$byYear, "study_period_by_year")
  write(period$range, "study_period_range")

  if (is.null(p2y12Records) && !is.null(connectionDetails)) {
    message("Read-only query of P2Y12 records")
    p2y12Records <- getP2y12Records(connectionDetails, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable,
                                    targetId, comparatorId)
  }
  if (!is.null(p2y12Records)) {
    message("Switching and discontinuation")
    profile <- switchingProfile(population, p2y12Records, targetId, comparatorId,
                                targetIngredient = p2y12Ingredients[["ticagrelor"]],
                                comparatorIngredient = p2y12Ingredients[["prasugrel"]],
                                minCellCount = minCellCount)
    write(profile$firstChange, "switching_first_change")
    write(profile$statusAt, "switching_status")
  }

  write(data.frame(
    item = c("R", "CohortMethod", "primaryAnalysisId", "onTreatmentAnalysisId", "minCellCount", "runTime"),
    value = c(R.version.string, as.character(packageVersion("CohortMethod")), primaryAnalysisId,
              onTreatmentAnalysisId, minCellCount, format(Sys.time()))
  ), "run_info")
  message("Revision post hoc outputs written to ", exportFolder)
  invisible(exportFolder)
}
