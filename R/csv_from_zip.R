#' @title Download a csv from a zip
#'
#' @description
#' `r lifecycle::badge('stable')`
#' A convenience function to allow you to pull data from NHS, ONS and ODR assets
#'
#' @param x a zip file from the web
#'
#' @return a zip file for ingestion into your chosen readr
#' @importFrom utils download.file unzip
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read.csv(csv_from_zip("https://files.digital.nhs.uk/assets/ods/current/pcodeall.zip"))
#' }
#'

csv_from_zip <- function(x) {

  loc.url <- x
  td <- tempdir()
  tf <- tempfile(tmpdir = td, fileext = ".zip")

  # Download with error handling
  tryCatch({
    utils::download.file(loc.url, tf, quiet = TRUE)
  }, error = function(e) {
    stop("Download failed: ", e$message)
  })

  # List files in ZIP
  files_in_zip <- tryCatch({
    unzip(tf, list = TRUE)$Name
  }, error = function(e) {
    stop("Failed to read ZIP: ", e$message)
  })

  # Added pattern to overcome case sensitivity
  pattern <- "\\.csv$"

  # Match pattern
  fname <- grep(pattern, files_in_zip, value = TRUE, ignore.case = TRUE)
  if (length(fname) == 0) {
    stop("No files matching pattern '", pattern, "' found in ZIP.")
  }

  # Extract file
  utils::unzip(tf, files = fname, exdir = td, overwrite = TRUE)

  fpath <- file.path(td, fname)
  return(fpath)

}

