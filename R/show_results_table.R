#' Show results table
#' Last updated: 2021-02-14
#'
#' @param data_table A tibble
#' @export

show_results_table <- function(data_table) {
  DT::datatable(data_table,
                filter = 'top',
                options = list(pageLength = 100,
                               autoWidth = TRUE))
}
