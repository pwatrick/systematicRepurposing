#' For drug repurposing EHR validation studies, map patient drugs to ingredients
#' Last updated: 2021-02-10
#'
#' @param .data A tibble
#' @export

map_drugs_to_ingredients <- function(.data, ...) {

  #Map drug exposures to RxCUI ingredients
  ##Map OHDSI concept ids to RxCUIs
  .data <- inner_join(.data, ddiwas::ddi_ohdsi_rxnorm, by = c("drug_concept_id" = "concept_id")) %>%
    select(person_id, rxcui, drug_exposure_start_date) %>%
    distinct()

  ##Map RxCUIs to RxCUI ingredients
  .data <- inner_join(.data, ddiwas::ddi_rxcui2in, by = "rxcui")
  .data <- .data %>%
    select(person_id, rxcui_ingr, rxcui_ingr_name, drug_exposure_start_date) %>%
    distinct()
}
