-- Abciximab drug records in the year before index, by source code (aggregate counts only)
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
	de.drug_source_value;
