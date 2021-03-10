#' Define baseline and treatment periods
#' Last updated: 2021-02-10
#'
#' Baseline period = "start_date" to "first_{drug}_exposure". Treatment period =
#' ("first_{drug}_exposure" + 30 days) to "last_{drug}_exposure". If treatment period > 12
#' months, then treatment period = ("first_{drug}_exposure" + 30 days) to "end_date"
#'
#'
#' @param .data A tibble
#' @export

define_observation_periods <- function(.data, ...) {

  #Calculate number of days between "first_drug_exposure" and "last_drug_exposure"
  .data$drug_length <- lubridate::interval(.data$first_drug_exposure,
                                           .data$last_drug_exposure)
  .data$drug_length <- lubridate::time_length(.data$drug_length, "day")

  #Remove patients with "drug_length" less than 30 days
  .data <- .data %>%
    dplyr::filter(drug_length >= 30)

  #If "last_drug_exposure" occurs before "end_date", set "final_end_date" to "last_drug_exposure", else set to "end_date"
  .data <- .data %>%
    dplyr::mutate(
      final_end_date = dplyr::if_else(last_drug_exposure <= end_date,
                                      last_drug_exposure,
                                      end_date)
    )

  #Get final columns of interest
  .data <- .data %>%
    dplyr::select(person_id, start_date, first_drug_exposure, final_end_date) %>%
    dplyr::distinct()

}
