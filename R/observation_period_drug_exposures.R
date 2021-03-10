#' Get baseline and treatment period drug exposures
#' Last updated: 2021-02-10
#'
#'
#' @param df_drug_obsperiod A tibble with columns, c('person_id','start_date','first_drug_exposure','final_end_date')
#' @param drugs A tibble with columns, c('person_id','rxcui_ingr','rxcui_ingr_name','drug_exposure_start_date')
#' @export

observation_period_drug_exposures <- function(df_drug_obsperiod, drugs,...) {

  #Get drugs that were in the baseline period
  df_baseline_period <- df_drug_obsperiod %>%
    select(person_id, start_date, first_drug_exposure) %>%
    distinct()
  df_baseline_period <- inner_join(df_baseline_period, drugs, by = "person_id")

  df_baseline_period <- df_baseline_period %>%
    filter(start_date <= drug_exposure_start_date & drug_exposure_start_date < first_drug_exposure) %>%
    select(person_id, rxcui_ingr, rxcui_ingr_name) %>%
    distinct()
  df_baseline_period$group <- "baseline"

  #Get drugs that were in the treatment period
  df_treatment_period <- df_drug_obsperiod %>%
    select(person_id, first_drug_exposure, final_end_date) %>%
    distinct()
  df_treatment_period <- inner_join(df_treatment_period, drugs, by = "person_id")
  df_treatment_period <- df_treatment_period %>%
    filter(first_drug_exposure <= drug_exposure_start_date & drug_exposure_start_date <= final_end_date) %>%
    select(person_id, rxcui_ingr, rxcui_ingr_name) %>%
    distinct()
  df_treatment_period$group <- "treatment"

  drugs_merged_periods <- bind_rows(df_treatment_period, df_baseline_period) %>%
    arrange(person_id, rxcui_ingr)

}
