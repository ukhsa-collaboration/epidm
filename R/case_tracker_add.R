#' Add cases to the tracking list
#'
#' Adds a list of unique case IDs to the tracking list with an initial status,
#' initializing the tracking list if it doesn’t exist.
#'
#' @param cases Character vector of unique case IDs or a data frame with a
#'   `case_id` column.
#' @param initial_status Character. The initial status for new cases
#'   (default is `"active"`).
#'
#' @return None. Updates the tracking list in the package environment.
#' @export
case_tracker_add <- function(cases, initial_status = "active") {
  # Initialize the tracking list if it doesn't already exist
  if (is.null(.tracking_env$tracking_list)) {
    .tracking_env$tracking_list <- data.frame(
      case_id = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
  }

  # Ensure cases is a character vector; if data frame, assume `case_id` column
  if (is.data.frame(cases)) {
    if (!"case_id" %in% names(cases)) {
      stop("Data frame must contain a 'case_id' column.")
    }
    cases <- as.character(cases$case_id)
  }

  # Add new cases to the tracking list
  new_entries <- data.frame(case_id = cases,
                            status = initial_status,
                            stringsAsFactors = FALSE)
  .tracking_env$tracking_list <- rbind(.tracking_env$tracking_list, new_entries)
}
