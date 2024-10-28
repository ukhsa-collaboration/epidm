#' Update the status of specified cases in the tracking list
#'
#' Updates the status of cases in the tracking list by their `case_id`.
#'
#' @param cases Character vector of case IDs or data frame with a `case_id`
#'   column.
#' @param new_status Character. Status message indicating the new state
#'   or reason for exclusion.
#'
#' @return None. Updates the status of specified cases in the tracking list.
#' @export
#'
case_tracker_update <- function(cases, new_status) {
  # Check that the tracking list exists
  if (is.null(.tracking_env$tracking_list)) {
    stop("Tracking list not initialized. Use case_tracker_add() to initialize.",)
  }

  # Ensure cases is a character vector; if data frame, assume `case_id` column
  if (is.data.frame(cases)) {
    if (!"case_id" %in% names(cases)) {
      stop("Data frame must contain a 'case_id' column.")
    }
    cases <- as.character(cases$case_id)
  }

  # Update status for specified cases
  case_indices <- which(.tracking_env$tracking_list$case_id %in% cases)
  if (length(case_indices) == 0) {
    warning("None of the specified cases were found in the tracking list.")
    return(invisible(NULL))
  }

  .tracking_env$tracking_list$status[case_indices] <- new_status
}
