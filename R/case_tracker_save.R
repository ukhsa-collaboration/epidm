#' Save the tracking list to a CSV file
#'
#' Saves the current tracking list to a specified file path as a CSV file.
#'
#' @param file_path Character. File path where the tracking list CSV should
#'   be saved.
#'
#' @return None. Saves the tracking list to a CSV file at the specified
#'   `file_path`.
#' @export
case_tracker_save <- function(file_path) {
  # Check that the tracking list exists
  if (is.null(.tracking_env$tracking_list)) {
    stop("Tracking list not initialized. Use case_tracker_add() to initialize.")
  }

  # Save the tracking list to a CSV file
  write.csv(.tracking_env$tracking_list, file_path, row.names = FALSE)
  message("Tracking list saved to ", file_path)
}
