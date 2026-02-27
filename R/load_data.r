#' @title read fixed width downloaded data
#' @details Uses output from `download_cdc()` to load the data into R as a tibble.
#' @param path the file path returned from `download_cdc()` or the path to the zip file downloaded.
#' @param meta the `data_` object with the metadata needed to read the fixed width file format.
#' @importFrom utils unzip
#' @export
load_data <- function(path, meta) {
  tempd <- tempdir()
  tempdf <- tempfile(tmpdir = tempd)
  dir.create(tempdf, showWarnings = FALSE)
  unzip(path, exdir = tempdf)
  fpath_name <- list.files(tempdf, full.names = TRUE)[1]
  data_out <- read_section(fpath_name, meta)
  unlink(tempdf)
  unlink(path)
  data_out
}
