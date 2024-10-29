#' Report the current tracking list or case count
#'
#' Returns the tracking list with case IDs and their current statuses or the
#' case count at different stages in the pipeline.
#'
#' @param type Character. Indicates whether to return `"case_list"` (default)
#'   or `"case_count"`.
#'
#' @return Data frame of the tracking list or case count.
#' @export
case_tracker_report <- function(type = "case_list") {
  if (type == "case_list") {
    if (is.null(.tracking_env$tracking_list)) {
      stop("Tracking list not initialized. Use case_tracker_add() to initialize.")
    }
    return(.tracking_env$tracking_list)
  } else if (type == "case_count") {
    if (is.null(.tracking_env$tracking_count)) {
      stop("Tracking count not initialized. Use case_tracker_add() to initialize.")
    }
    return(.tracking_env$tracking_count)
  } else {
    stop("Invalid type specified. Use 'case_list' or 'case_count'.")
  }
}
