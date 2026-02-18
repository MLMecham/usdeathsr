#' CDC Vital Statistics file links
#'
#' A dataset containing all scraped file links from the CDC Vital Statistics
#' Online page and the Mortality Multiple Cause-of-Death documentation page.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{section}{Section name}
#'   \item{subsection}{Subsection name}
#'   \item{link_text}{Text of the download link}
#'   \item{year}{Four-digit year as integer}
#'   \item{file_size}{Raw file size string}
#'   \item{url}{Absolute URL to the file}
#'   \item{file_type}{File extension}
#'   \item{file_size_mb}{File size in megabytes}
#' }
#'
#' @source \url{https://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm}
"all_cdc_data"
