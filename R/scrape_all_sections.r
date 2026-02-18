# Tell R CMD check these are column names to avoid notes
utils::globalVariables(c("link_text", "subsection", "year", "file_size", "file_type", "section", "file_size_mb"))

#' Scrape all CDC Vital Statistics sections
#'
#' Downloads and combines all the main CDC Vital Statistics sections
#' into a single tibble. Optionally scrapes the separate Mortality Multiple
#' Cause-of-Death documentation page and merges it in.
#'
#' @param url Character string with the CDC Vital Stats page URL.
#'   Typically \url{https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm}.
#' @param url_pdf Optional character string with the CDC Mortality documentation
#'   page URL. If provided, the placeholder mortality user guide link is replaced
#'   with the full set of scraped links from that page.
#'   Typically \url{https://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{section}{Section name}
#'   \item{subsection}{Subsection name}
#'   \item{link_text}{Text of the download link}
#'   \item{year}{Four-digit year as integer}
#'   \item{file_size}{Raw file size string}
#'   \item{url}{Absolute URL to the file}
#'   \item{file_type}{File extension}
#'   \item{file_size_mb}{File size converted to megabytes}
#' }
#'
#' @details
#' When \code{url_pdf} is provided, the single placeholder link for the
#' mortality multiple cause-of-death user guide is removed and replaced with
#' the full set of file links scraped from the mortality documentation page.
#'
#' A known typo in the CDC source data causes one file size to appear as
#' \code{"10.2.MB"} instead of \code{"10.2 MB"}. This is corrected automatically.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' data <- scrape_all_sections("https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm")
#'
#' # With full mortality documentation
#' data <- scrape_all_sections(
#'     url     = "https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm",
#'     url_pdf = "https://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm"
#' )
#' }
#'
#' @importFrom dplyr bind_rows mutate across everything filter
#' @importFrom stringr str_replace str_sub
#' @importFrom purrr imap_dfr
#' @importFrom magrittr %>%
#' @export
scrape_all_sections <- function(url, url_pdf = NULL) {
    page <- get_html_page(url)

    sections <- list(
        Births              = c("User Guide", "U.S. Data", "U.S. Territories"),
        Period_cohort       = c("User Guide", "U.S. Data", "U.S. Territories"),
        Birth_Cohort        = c("User Guide", "U.S. Data", "U.S. Territories"),
        `matched-multiple`  = c("User Guide", "U.S. Data"),
        Mortality_Multiple  = c("User Guide", "U.S. Data", "U.S. Territories"),
        Fetal_Death         = c("User Guide", "U.S. Data", "U.S. Territories")
    )

    result <- purrr::imap_dfr(sections, function(subs, anchor_id) {
        scrape_cdc_section(page, anchor_id, tolower(anchor_id), subs) %>%
            dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }) %>%
        mutate(
            # Fixes a typo in CDC source data where decimal numbers before units are malformed
            # e.g. "10.2.MB" becomes "10.2 MB"
            file_size    = str_replace(file_size, "([\\d]+\\.[\\d]+)\\.", "\\1 "),
            year         = str_sub(year, 1, 4) %>% as.integer(),
            file_size_mb = parse_file_size_mb(file_size)
        )

    if (!is.null(url_pdf)) {
        result <- result %>%
            filter(!(section == "mortality_multiple" & subsection == "User Guide" & file_type == ".htm")) %>%
            bind_rows(scrape_mult_mort_user_guide(url_pdf))
    }

    result
}
