#' Save the tracking list or case count to a CSV file
#'
#' Saves the current tracking list or case count to a specified file path as a
#' CSV file.
#'
#' @param file_path Character. File path where the CSV should be saved.
#' @param type Character. Indicates whether to save `"case_list"` (default)
#'   or `"case_count"`.
#'
#' @return None. Saves the tracking list or case count to a CSV file.
#' @export
case_tracker_save <- function(file_path, type = "case_list") {
  if (type == "case_list") {
    if (is.null(.tracking_env$tracking_list)) {
      stop("Tracking list not initialized. Use case_tracker_add() to initialize.")
    }
    write.csv(.tracking_env$tracking_list, file_path, row.names = FALSE)
  } else if (type == "case_count") {
    if (is.null(.tracking_env$tracking_count)) {
      stop("Tracking count not initialized. Use case_tracker_add() to initialize.")
    }
    write.csv(.tracking_env$tracking_count, file_path, row.names = FALSE)
  } else {
    stop("Invalid type specified. Use 'case_list' or 'case_count'.")
  }

  message("Data saved to ", file_path)
}
