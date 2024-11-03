#' Update the case status based on the current case list at a checkpoint
#'
#' Updates the case status in the tracking list based on the provided list of
#' current case IDs at a specific checkpoint in the pipeline. Only cases that
#' are missing from the current list will be marked as dropped, while all
#' other cases retain their specified status.
#'
#' @param cases A character vector of the current case IDs still in the pipeline.
#' @param checkpoint Character. The name of the checkpoint in the data pipeline.
#' @param type Character. Indicates whether to update `"case_list"` (default)
#'   or `"case_count"`.
#' @param active_status Character. The status to assign to cases that remain in the pipeline at this checkpoint.
#'   Defaults to "active".
#'
#' @return None. Updates the status of cases that are dropped at this checkpoint.
#' @export
case_tracker_update <- function(cases,
                                checkpoint,
                                type = "case_list",
                                active_status = checkpoint) {
  if (type == "case_list") {
    # Check that tracking_list exists
    if (is.null(.tracking_env$tracking_list)) {
      stop("Tracking list not initialized. Use case_tracker_add() to initialize.")
    }

    # Ensure cases is a character vector
    if (!is.character(cases)) {
      stop("For 'case_list', cases should be a character vector of current case IDs.")
    }

    # Identify previous cases in the tracking list
    previous_cases <- .tracking_env$tracking_list$case_id

    # 1. Mark dropped cases (present in previous_cases but not in cases)
    dropped_cases <- setdiff(previous_cases, cases)
    drop_message <- paste("Dropped at", checkpoint)
    if (length(dropped_cases) > 0) {
      for (case_id in dropped_cases) {
        # Only mark as dropped if no previous drop message exists
        current_status <- .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == case_id]
        if (!grepl("^Dropped at", current_status)) {
          # Prevent overwriting previous drop statuses
          .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id == case_id] <- drop_message
        }
      }
    }

    # 2. Set status of remaining cases to the specified active_status (e.g., checkpoint)
    active_cases <- intersect(previous_cases, cases)
    if (length(active_cases) > 0) {
      .tracking_env$tracking_list$status[.tracking_env$tracking_list$case_id %in% active_cases] <- active_status
    }

    # 3. Add new cases (present in cases but not in previous_cases) with the checkpoint status
    new_cases <- setdiff(cases, previous_cases)
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

    # Log the case count with the checkpoint name
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
