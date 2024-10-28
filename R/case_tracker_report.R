#' Report the current tracking list
#'
#' Returns the tracking list with case IDs and their current statuses.
#'
#' @return Data frame of the tracking list with columns `case_id` and `status`.
#' @export
case_tracker_report <- function() {
  # Check that the tracking list exists
  if (is.null(.tracking_env$tracking_list)) {
    stop("Tracking list not initialized. Use case_tracker_add() to initialize.")
  }

  # Return the tracking list
  return(.tracking_env$tracking_list)
}
