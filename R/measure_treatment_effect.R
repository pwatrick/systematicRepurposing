#' Measure treatment effect in clinical data
#' Last updated: 2021-02-12
#'
#'
#'
#'
#' @param .data A tibble with columns, c("person_id", "drug_exposed", "treatment_new_exposed")
#' @param biomarker_tbl A tibble with columns, c("person_id", "biomarker_baseline_value", "biomarker_treatment_value", "biomarker_change", "biomarker_name")
#' @param drug_name A string, e.g., "Lisinopril"
#' @param phenotype A string, e.g., "Hyperlipidemia"
#' @param biomarker_name A string, e.g., "LDL cholesterol"
#' @param cohort_vector A vector, e.g,. c("Primary Analysis", "Subanalysis 1", "Subanalysis 2", "Subanalysis 3")
#' @export

measure_treatment_effect <- function(.data, biomarker_tbl, drug_name, phenotype, biomarker_name, cohort_vector) {

  #Primary Analysis
  cohort_flagged <- inner_join(biomarker_tbl, .data, by = "person_id")

  measurement_test <- t.test(cohort_flagged$biomarker_treatment_value,
                             cohort_flagged$biomarker_baseline_value,
                             paired = TRUE,
                             alternative = 'less')

  cohort_type <- cohort_vector[1]

  primary_analysis <- tibble(
    drug = drug_name,
    cohort = cohort_vector[1],
    phenotype = phenotype,
    biomarker = biomarker_name,
    mean_baseline = mean(cohort_flagged$biomarker_baseline_value),
    sd_baseline = sd(cohort_flagged$biomarker_baseline_value),
    mean_treatment = mean(cohort_flagged$biomarker_treatment_value),
    sd_treatment = sd(cohort_flagged$biomarker_treatment_value),
    mean_change = mean(cohort_flagged$biomarker_change),
    sd_change = sd(cohort_flagged$biomarker_change),
    patient_number = n_distinct(cohort_flagged$person_id),
    pval = measurement_test$p.value
  )

  #Subanalysis 1
  cohort_flagged_subanalysis1 <- cohort_flagged %>% filter(drug_exposed == 0 & treatment_new_exposed == 0)

  measurement_test_sa1 <- t.test(cohort_flagged_subanalysis1$biomarker_treatment_value,
                                 cohort_flagged_subanalysis1$biomarker_baseline_value,
                                 paired = TRUE,
                                 alternative = 'less')

  subanalysis_1 <- tibble(
    drug = drug_name,
    cohort = cohort_vector[2],
    phenotype = phenotype,
    biomarker = biomarker_name,
    mean_baseline = mean(cohort_flagged_subanalysis1$biomarker_baseline_value),
    sd_baseline = sd(cohort_flagged_subanalysis1$biomarker_baseline_value),
    mean_treatment = mean(cohort_flagged_subanalysis1$biomarker_treatment_value),
    sd_treatment = sd(cohort_flagged_subanalysis1$biomarker_treatment_value),
    mean_change = mean(cohort_flagged_subanalysis1$biomarker_change),
    sd_change = sd(cohort_flagged_subanalysis1$biomarker_change),
    patient_number = n_distinct(cohort_flagged_subanalysis1$person_id),
    pval = measurement_test_sa1$p.value
  )

  merged_results <- bind_rows(primary_analysis, subanalysis_1)

  #Subanalysis 2
  cohort_flagged_subanalysis2 <- cohort_flagged %>% filter(drug_exposed == 1 & treatment_new_exposed == 0)

  measurement_test_sa2 <- t.test(cohort_flagged_subanalysis2$biomarker_treatment_value,
                                 cohort_flagged_subanalysis2$biomarker_baseline_value,
                                 paired = TRUE,
                                 alternative = 'less')

  subanalysis_2 <- tibble(
    drug = drug_name,
    cohort = cohort_vector[3],
    phenotype = phenotype,
    biomarker = biomarker_name,
    mean_baseline = mean(cohort_flagged_subanalysis2$biomarker_baseline_value),
    sd_baseline = sd(cohort_flagged_subanalysis2$biomarker_baseline_value),
    mean_treatment = mean(cohort_flagged_subanalysis2$biomarker_treatment_value),
    sd_treatment = sd(cohort_flagged_subanalysis2$biomarker_treatment_value),
    mean_change = mean(cohort_flagged_subanalysis2$biomarker_change),
    sd_change = sd(cohort_flagged_subanalysis2$biomarker_change),
    patient_number = n_distinct(cohort_flagged_subanalysis2$person_id),
    pval = measurement_test_sa2$p.value
  )

  merged_results <- bind_rows(merged_results, subanalysis_2)

  #Subanalysis 3
  cohort_flagged_subanalysis3 <- bind_rows(cohort_flagged_subanalysis1, cohort_flagged_subanalysis2)

  measurement_test_sa3 <- t.test(cohort_flagged_subanalysis3$biomarker_treatment_value,
                                 cohort_flagged_subanalysis3$biomarker_baseline_value,
                                 paired = TRUE,
                                 alternative = 'less')

  subanalysis_3 <- tibble(
    drug = drug_name,
    cohort = cohort_vector[4],
    phenotype = phenotype,
    biomarker = biomarker_name,
    mean_baseline = mean(cohort_flagged_subanalysis3$biomarker_baseline_value),
    sd_baseline = sd(cohort_flagged_subanalysis3$biomarker_baseline_value),
    mean_treatment = mean(cohort_flagged_subanalysis3$biomarker_treatment_value),
    sd_treatment = sd(cohort_flagged_subanalysis3$biomarker_treatment_value),
    mean_change = mean(cohort_flagged_subanalysis3$biomarker_change),
    sd_change = sd(cohort_flagged_subanalysis3$biomarker_change),
    patient_number = n_distinct(cohort_flagged_subanalysis3$person_id),
    pval = measurement_test_sa3$p.value
  )

  merged_results <- bind_rows(merged_results, subanalysis_3)

  return(merged_results)
}
