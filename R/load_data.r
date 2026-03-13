#' @title Load CDC Fixed-Width Data
#' @details Uses output from `download_cdc()` to load the data into R as a tibble.
#'   For most datasets this returns a single tibble. For linked birth/infant death
#'   cohort data (identified automatically from the URL), it returns a named list
#'   with two elements: \code{denominator} (all births) and \code{numerator}
#'   (linked infant death records only). See \code{load_data_birth_cohort()} for
#'   details on the den/num structure.
#'
#'   CDC natality files from 1968-1972 are distributed as Microsoft Publisher
#'   (.pub) files and require LibreOffice to convert. Conversion is handled
#'   automatically but takes 10-20 minutes. See \code{convert_pub()} for details.
#' @param path the file path returned from `download_cdc()` or the URL to the zip file.
#' @param meta the `data_` object with the metadata needed to read the fixed width file format.
#' @param soffice Character. Path to the LibreOffice executable. Only required
#'   for 1968-1972 natality files. Defaults to the standard Windows installation
#'   path. Linux/Mac users will need to override this (e.g.
#'   \code{"/usr/bin/soffice"} on Linux or
#'   \code{"/Applications/LibreOffice.app/Contents/MacOS/soffice"} on Mac).
#' @return A tibble for standard datasets, or a named list with \code{denominator}
#'   and \code{numerator} tibbles for linked birth cohort data.
#' @importFrom utils unzip
#' @export
load_data <- function(path, meta, soffice = "C:/Program Files/LibreOffice/program/soffice.exe") {
  if (grepl("cohortlinkedus", path)) {
    return(load_data_birth_cohort(path, meta))
  }

  tempd <- tempdir()
  tempdf <- tempfile(tmpdir = tempd)
  dir.create(tempdf, showWarnings = FALSE)
  unzip(path, exdir = tempdf)

  fpath_name <- list.files(tempdf, full.names = TRUE)[1]

  if (grepl("\\.pub$", fpath_name, ignore.case = TRUE)) {
    fpath_name <- convert_pub(fpath_name, tempdf, soffice)
  }

  data_out <- read_section(fpath_name, meta)
  unlink(tempdf, recursive = TRUE)
  unlink(path)
  data_out
}
