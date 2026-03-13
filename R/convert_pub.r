#' Convert a Microsoft Publisher file to plain text
#'
#' Uses LibreOffice to convert a `.pub` file to plain text. This is required
#' for CDC natality files from 1968-1972 which are distributed as Microsoft
#' Publisher files. Conversion typically takes 10-20 minutes depending on
#' file size and machine speed.
#'
#' @param pub_path Character. Full path to the `.pub` file to convert.
#' @param outdir Character. Directory to write the converted `.txt` file to.
#' @param soffice Character. Path to the LibreOffice executable. Defaults to
#'   the standard Windows installation path. Linux/Mac users will need to
#'   override this (e.g. \code{"/usr/bin/soffice"} on Linux or
#'   \code{"/Applications/LibreOffice.app/Contents/MacOS/soffice"} on Mac).
#'
#' @return Character. Full path to the converted `.txt` file.
#'
#' @note LibreOffice must be installed on your system to use this function.
#'   Download it at \url{https://www.libreoffice.org/download/libreoffice-fresh/}.
#'   To avoid the conversion step entirely, pre-converted versions of the
#'   1968-1972 natality files are available as a GitHub release. See the
#'   package README for details.
#'
#' @keywords internal
convert_pub <- function(pub_path, outdir, soffice = "C:/Program Files/LibreOffice/program/soffice.exe") {
    txt_path <- file.path(outdir, gsub("\\.pub$", ".txt", basename(pub_path), ignore.case = TRUE))
    input_size <- file.size(pub_path)

    cli::cli_alert_info("Converting .pub to .txt via LibreOffice...")
    start_time <- Sys.time()

    px <- processx::process$new(
        command = soffice,
        args = c("--headless", "--convert-to", "txt", "--outdir", outdir, pub_path)
    )

    # Spinner while LibreOffice parses the .pub file
    sp <- cli::cli_progress_bar(
        name = "Parsing .pub file",
        total = NA,
        format = "{cli::pb_spin} {cli::pb_name} | elapsed: {cli::pb_elapsed}"
    )

    while (px$is_alive() && !file.exists(txt_path)) {
        Sys.sleep(0.5)
        cli::cli_progress_update(id = sp)
    }
    cli::cli_progress_done(id = sp)

    # Progress bar once txt file starts writing
    pb <- cli::cli_progress_bar(
        name = "Writing .txt",
        total = 100,
        format = "{cli::pb_bar} {cli::pb_percent} | elapsed: {cli::pb_elapsed}"
    )

    while (px$is_alive()) {
        Sys.sleep(0.5)
        pct <- min(round((file.size(txt_path) / input_size) * 100), 99)
        cli::cli_progress_update(id = pb, set = pct)
    }

    cli::cli_progress_update(id = pb, set = 100)
    cli::cli_progress_done(id = pb)

    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    cli::cli_alert_success("Done! Conversion took {elapsed} seconds.")

    txt_path
}
