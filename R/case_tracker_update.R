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
#' @return None. Updates the status of cases to the specified checkpoint or marks cases as dropped.
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

    # Get all previously tracked cases
    previous_cases <- .tracking_env$tracking_list$case_id

    # Identify removed cases by comparing previous and current lists
    removed_cases <- setdiff(previous_cases, cases)

    # Set a drop message for cases removed at this checkpoint
    drop_message <- paste("Dropped at", checkpoint)

    # Update status to "Dropped at <checkpoint>" for cases removed at this checkpoint
    for (case_id in removed_cases) {
      current_status <- .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == case_id]

      # Update only if the current status is not already marked as dropped
      if (!grepl("Dropped at", current_status)) {
        .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == case_id] <- drop_message
      }
    }

    # Update the status of remaining cases to the checkpoint name
    active_indices <- which(.tracking_env$tracking_list$case_id %in% cases)
    .tracking_env$tracking_list$status[active_indices] <- checkpoint

    # Add any cases that are not already in tracking_list
    new_cases <- cases[!cases %in% previous_cases]
    if (length(new_cases) > 0) {
      new_entries <- data.frame(
        case_id = new_cases,
        status = checkpoint,
        stringsAsFactors = FALSE
      )
      .tracking_env$tracking_list <- rbind(.tracking_env$tracking_list, new_entries)
    }
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
