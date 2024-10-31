#' Clear the case list and case count tracking tables
#'
#' Clears the `tracking_list` and `tracking_count` in the environment, effectively
#' resetting the tracking data to start fresh for a new pipeline run or region.
#'
#' @return None. Resets the `tracking_list`, `tracking_count`, and `last_status`.
#' @export
case_tracker_clear <- function() {
  .tracking_env$tracking_list <- NULL  # Reset case list tracking
  .tracking_env$tracking_count <- NULL  # Reset case count tracking
  .tracking_env$last_status <- NULL  # Reset last status to NULL, for fresh checkpoint tracking
  message("Case tracker has been cleared.")
}
