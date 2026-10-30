-- P2Y12 inhibitor exposures of the study patients, day -30 to 365 around index (one row
-- per drug exposure); the ingredient may differ by arm. Used for the switching profile.
DELETE FROM @target_database_schema.@target_cohort_table WHERE cohort_definition_id = @target_cohort_id;

INSERT INTO @target_database_schema.@target_cohort_table (
	cohort_definition_id,
	subject_id,
	cohort_start_date,
	cohort_end_date
)
SELECT DISTINCT @target_cohort_id AS cohort_definition_id,
	de.person_id AS subject_id,
	de.drug_exposure_start_date AS cohort_start_date,
	COALESCE(de.drug_exposure_end_date,
		DATEADD(day, CASE WHEN de.days_supply > 0 THEN de.days_supply - 1 ELSE 0 END, de.drug_exposure_start_date)) AS cohort_end_date
FROM @cdm_database_schema.drug_exposure de
INNER JOIN @vocabulary_database_schema.concept_ancestor ca
	ON ca.descendant_concept_id = de.drug_concept_id
INNER JOIN (
	SELECT subject_id, cohort_definition_id, MIN(cohort_start_date) AS index_date
	FROM @study_cohort_database_schema.@study_cohort_table
	WHERE cohort_definition_id IN (@target_id, @comparator_id)
	GROUP BY subject_id, cohort_definition_id
) sp
	ON sp.subject_id = de.person_id
WHERE ((sp.cohort_definition_id = @target_id AND ca.ancestor_concept_id = @target_arm_ingredient_id)
		OR (sp.cohort_definition_id = @comparator_id AND ca.ancestor_concept_id = @comparator_arm_ingredient_id))
	AND de.drug_exposure_start_date >= DATEADD(day, -30, sp.index_date)
	AND de.drug_exposure_start_date <= DATEADD(day, 365, sp.index_date);
