#' @title download cdc data object
#' @description Uses the `cdc_link_lookup` data frame.
#' @param url is a cdc url path that can be found from `cdc_link_lookup` variable `us_data_url` path as `url` input.
#' @return the temporary file where the file was downloaded.
#' @examples
#' #'
#' \dontrun{
#' meta <- data_mortality_multiple_1969
#' download <- usdeaths::cdc_link_lookup
#' url <- download |>
#'   filter(section == "mortality_multiple" & year == 1969) |>
#'   pull(us_data_url)
#' path <- download_cdc(url)
#' }
#' @import httr2
#' @export
download_cdc <- function(url) {
  temp <- tempfile(fileext = ".zip")
  httr2::request(url) |>
    httr2::req_progress() |>
    httr2::req_perform(path = temp)
  temp
}
