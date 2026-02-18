#' Scrape Mortality Multiple Cause-of-Death user guide links
#'
#' Extracts downloadable file links from the CDC Mortality Multiple
#' Cause-of-Death documentation page and returns a tidy tibble with
#' metadata about each file.
#'
#' @param url URL of the CDC mortality documentation page.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{section}{Always "mortality_multiple"}
#'   \item{subsection}{Always "User Guide"}
#'   \item{link_text}{Text of the download link}
#'   \item{year}{Four-digit year extracted from link text, filled down for sub-items}
#'   \item{file_size}{File size string, if present}
#'   \item{url}{Absolute URL to the file}
#'   \item{file_type}{File extension}
#'   \item{file_size_mb}{File size converted to megabytes}
#' }
#'
#' @details
#' This function is designed for the CDC Mortality Multiple Cause-of-Death
#' documentation page at:
#' \url{https://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm}
#'
#' Note: 1997 and 1998 entries link to separate HTML pages containing many
#' PDFs and are not scraped by this function as of 2/17/2026.
#'
#' @importFrom rvest html_elements html_text2 html_attr url_absolute
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr fill
#' @importFrom stringr str_extract str_remove_all
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @export
scrape_mult_mort_user_guide <- function(url) {
    page <- get_html_page(url)

    links <- page %>% html_elements(".cdc-textblock li a[href]")

    tibble(
        link_text = html_text2(links),
        url       = html_attr(links, "href")
    ) %>%
        filter(!is.na(url)) %>%
        mutate(
            url = url_absolute(url, "https://www.cdc.gov"),
            file_type = str_extract(url, "\\.[a-zA-Z0-9]+$"),
            file_size = str_extract(link_text, "\\[PDF \\u2013 ([^]]+)\\]") %>%
                str_remove_all("\\[PDF \\u2013 |\\]"),
            year = as.integer(str_extract(link_text, "\\d{4}")),
            file_size_mb = parse_file_size_mb(file_size),
            section = "mortality_multiple",
            subsection = "User Guide"
        ) %>%
        fill(year, .direction = "down") %>%
        select(section, subsection, link_text, year, file_size, url, file_type, file_size_mb)
}
