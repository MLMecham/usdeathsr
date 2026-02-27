#' Decode all coded columns in a mortality dataset
#'
#' Decodes all coded columns in a mortality dataset using a metadata table,
#' replacing coded values with human-readable labels in place. Unlike
#' \code{decode_preview()}, this function processes the entire dataset and
#' is intended for production use.
#'
#' @param data A data frame of mortality records, typically loaded via
#'   \code{load_data()} or \code{cdc_import()}.
#' @param meta A metadata tibble with at minimum columns \code{name} and
#'   \code{codes}, such as \code{data_mortality_multiple_1969}.
#'
#' @return A tibble with all columns in metadata order, with coded columns
#'   decoded in place.
#'
#' @examples
#' \dontrun{
#' meta <- data_mortality_multiple_1969
#' mort1969 <- cdc_import("mortality_multiple", 1969)
#' mort1969_decoded <- decode_all(mort1969, meta)
#' }
#'
#' @importFrom dplyr mutate across all_of cur_column select
#' @export
decode_all <- function(data, meta) {
    cols <- meta$name[meta$codes != ""]

    data |>
        mutate(across(
            all_of(cols),
            ~ decode_column(., meta$codes[meta$name == cur_column()])
        )) |>
        select(all_of(meta$name))
}
