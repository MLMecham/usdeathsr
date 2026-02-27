#' Get CDC data URL for a given section and year
#'
#' Looks up the US data download URL from the internal CDC link table for a
#' given section and year combination.
#'
#' @param sec A string identifying the data section, e.g. \code{"mortality_multiple"}.
#' @param yr An integer or numeric year, e.g. \code{1969}.
#'
#' @return A character string containing the URL to the zip file on the CDC FTP server.
#'
#' @examples
#' \dontrun{
#' get_cdc_url("mortality_multiple", 1969)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @export
get_cdc_url <- function(sec, yr) {
    matched <- cdc_link_lookup |>
        filter(.data$section == sec, .data$year == yr)

    if (nrow(matched) == 0) {
        stop("No URL found for section '", sec, "' and year ", yr, ".")
    }

    matched$us_data_url
}
