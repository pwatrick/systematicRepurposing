#' Processes clinical data and outputs table summarizing treatment effect and number of overlapping patients in raw data
#' Last updated: 2021-02-17
#'
#' @param drug A string, e.g., "simvastatin"
#' @param phenotype A string, e.g., "Hyperlipidemia"
#' @param biomarker A string, e.g., "LDL Cholesterol"
#' @param indication_drugs A tibble with columns c('rxcui_in', 'drug_desc', 'indication'), e.g.,  systematicRepurposing::lipid_drugs_20201217141539
#' @param concept_id_exclusions A vector, e.g., c(2212451)
#' @param r_covariates A tibble with columns c('person_id','dob','gender_concept_id','race_concept_id','drug_concept_id','first_drug_exposure','last_drug_exposure')
#' @param r_drugs A tibble with columns c('person_id','start_date','end_date','drug_exposure_start_date','drug_concept_id')
#' @param r_biomarkers A tibble with columns c('person_id','start_date','measurement_date','end_date','measurement_concept_id','value_as_number')
#' @export

analyze_clinical_data <- function(drug, phenotype, biomarker, indication_drugs, concept_id_exclusions,
                                  r_covariates, r_drugs, r_biomarkers) {
  #Make sure that patients are in all three tables
  r_biomarker_temp <- r_biomarkers %>%
    select(person_id) %>% distinct() %>%
    rename(biomarker_person_id = person_id) %>%
    mutate(biomarker_tbl = 1)
  r_covariates_temp <- r_covariates %>%
    select(person_id) %>% distinct() %>%
    rename(covariates_person_id = person_id) %>%
    mutate(covariates_tbl = 1)
  r_drugs_temp <- r_drugs %>%
    select(person_id) %>%
    distinct() %>%
    rename(drugs_person_id = person_id) %>% mutate(drugs_tbl = 1)
  r_merged <- inner_join(r_biomarker_temp, r_drugs_temp, by = c("biomarker_person_id" = "drugs_person_id"))
  r_merged <- inner_join(r_merged, r_covariates_temp, by = c("biomarker_person_id" = "covariates_person_id"))

  r_merged %>% group_by(biomarker_tbl, drugs_tbl, covariates_tbl) %>% tally()
  overlapping_patients <- r_merged$biomarker_person_id
  ##Store number of overlapping patients
  overlapping_patient_count <- n_distinct(r_merged$biomarker_person_id)

  rm(r_biomarker_temp, r_covariates_temp, r_drugs_temp, r_merged); gc()

  #Remove patients that are not in all three tables
  r_covariates <- r_covariates %>% filter(person_id %in% overlapping_patients)
  r_drugs <- r_drugs %>% filter(person_id %in% overlapping_patients)
  r_biomarkers <- r_biomarkers %>% filter(person_id %in% overlapping_patients)

  #Define treatment and baseline periods
  p_obsperiod1 <- r_drugs %>%
    select(person_id, start_date, end_date) %>%
    distinct()
  p_obsperiod2 <- r_covariates %>%
    select(person_id, drug_concept_id, first_drug_exposure, last_drug_exposure) %>%
    distinct()
  p_obsperiod3 <- inner_join(p_obsperiod1, p_obsperiod2, by = "person_id")
  f_obsperiod <- p_obsperiod3 %>%
    systematicRepurposing::define_observation_periods()
  f_obsperiod <- f_obsperiod %>%
    filter(person_id %in% overlapping_patients)
  rm(p_obsperiod1, p_obsperiod2, p_obsperiod3); gc()

  #Map drugs to ingredients
  p_drugs <- r_drugs %>%
    select(person_id, drug_exposure_start_date, drug_concept_id) %>%
    distinct()
  f_drugs <- p_drugs %>%
    systematicRepurposing::map_drugs_to_ingredients()
  rm(r_drugs, p_drugs); gc()

  #Process covariates
  p_covariates <- r_covariates %>%
    select(person_id, dob, gender_concept_id, race_concept_id)
  ##Try keeping all patients regardless of race
  f_covariates <- systematicRepurposing::process_covariates(p_covariates, f_obsperiod, european = 0)

  rm(p_covariates, r_covariates); gc()

  #Get drug exposures in baseline and treatment periods
  f_drugs_periods <-
    systematicRepurposing::observation_period_drug_exposures(f_obsperiod, f_drugs)

  #Remove `drug` from indication drugs
  indication_drugs <- indication_drugs %>%
    filter(!str_detect(drug_desc, drug))
  #Get indication drug exposed patients
  f_indication_drug_exposed <- f_drugs_periods %>%
    filter(rxcui_ingr %in% indication_drugs$rxcui_in)

  #Flag patients for subanalyses, stratified on exposure to approved drugs for target disease
  f_covariates_flagged <-
    systematicRepurposing::add_subanalysis_flags(f_indication_drug_exposed, f_covariates)

  #Process biomarker data
  p_biomarkers <-
    systematicRepurposing::process_biomarker(biomarker_tbl = r_biomarkers,
                                             biomarker_name = biomarker,
                                             covariates_flagged = f_covariates_flagged,
                                             baseline_cols = c('person_id', 'start_date',
                                                               'first_drug_exposure', 'final_end_date'),
                                             inclusion_cols = c('person_id', 'measurement_date',
                                                                'measurement_concept_id', 'value_as_number'),
                                             concept_id_exclusions = concept_id_exclusions)

  #Hypothesis testing
  drug_subcohorts <- f_covariates_flagged %>%
    select(person_id, drug_exposed, treatment_new_exposed) %>%
    distinct()

  clinical_data_drug_results <- drug_subcohorts %>%
    systematicRepurposing::measure_treatment_effect(
      biomarker_tbl = p_biomarkers,
      drug_name = drug,
      phenotype = phenotype,
      biomarker_name = biomarker,
      cohort_vector = c("Primary Analysis", "Subanalysis 1", "Subanalysis 2", "Subanalysis 3")
    )

  data_list <- list(clinical_data_drug_results, overlapping_patient_count)
  names(data_list) <- c("clinical_data_drug_results", "overlapping_patient_count")
  return(data_list)
}
