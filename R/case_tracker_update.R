#' Update the case status based on the current case list at a checkpoint
#'
#' Updates the case status in the tracking list based on the provided list of
#' current case IDs at a specific checkpoint in the pipeline.
#'
#' @param cases A character vector of the current case IDs still in the pipeline.
#' @param checkpoint Character. The name of the checkpoint in the data pipeline.
#' @param type Character. Indicates whether to update `"case_list"` (default)
#'   or `"case_count"`.
#'
#' @return None. Updates the status of cases to "active" or "Dropped at <checkpoint>".
#' @export
case_tracker_update <- function(cases, checkpoint, type = "case_list") {
  if (type == "case_list") {
    # Check that tracking_list exists
    if (is.null(.tracking_env$tracking_list)) {
      stop("Tracking list not initialized. Use case_tracker_add() to initialize.")
    }

    # Ensure cases is a character vector
    if (!is.character(cases)) {
      stop("For 'case_list', cases should be a character vector of current case IDs.")
    }

    # Initialize last_status in the environment if not set
    if (is.null(.tracking_env$last_status)) {
      .tracking_env$last_status <- "active"
    }

    # Get all previously tracked cases
    previous_cases <- .tracking_env$tracking_list$case_id

    # Identify removed cases by comparing previous and current lists
    removed_cases <- setdiff(previous_cases, cases)

    # Warning if none of the provided cases match any in tracking_list
    if (length(intersect(previous_cases, cases)) == 0) {
      warning("None of the specified cases were found in the tracking list.")
    }

    # Update status to "Dropped at <checkpoint>" for cases removed at this checkpoint
    drop_message <- paste("Dropped at", checkpoint)
    for (case_id in removed_cases) {
      # Only update if the status is still "active" (i.e., it hasn't been dropped before)
      current_status <- .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == case_id]
      if (current_status == "active") {
        .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == case_id] <- drop_message
      }
    }

    # Update the status of remaining cases to "active"
    active_indices <- which(.tracking_env$tracking_list$case_id %in% cases)
    .tracking_env$tracking_list$status[active_indices] <- "active"

    # Update the last status message to the checkpoint name (for readability in debugging)
    .tracking_env$last_status <- checkpoint
  } else if (type == "case_count") {
    # Check that tracking_count exists
    if (is.null(.tracking_env$tracking_count)) {
      stop("Tracking count not initialized. Use case_tracker_add() to initialize.")
    }

    # Add the new case count to the tracking count with the checkpoint name
    new_entry <- data.frame(
      case_count = cases,
      status = checkpoint,
      stringsAsFactors = FALSE
    )
    .tracking_env$tracking_count <- rbind(.tracking_env$tracking_count, new_entry)
  } else {
    stop("Invalid type specified. Use 'case_list' or 'case_count'.")
  }
}
