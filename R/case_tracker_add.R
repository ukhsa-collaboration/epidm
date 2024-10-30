#' Add or initialize cases in the tracking list or initialize case count
#'
#' Adds cases to the tracking list with a user-defined initial status, or
#' initializes the case count with a specified count value at the start
#' of the pipeline. This function can also be used to add additional cases later.
#'
#' @param cases A character vector of unique case IDs if `type = "case_list"`,
#'   or an integer case count if `type = "case_count"`.
#' @param status Character. The status to assign to new cases (default: "initial").
#' @param type Character. Indicates whether to update `"case_list"` (default)
#'   or `"case_count"`.
#'
#' @return None. Updates the tracking list or initializes the case count.
#' @export
case_tracker_add <- function(cases,
                             status = "initial",
                             type = "case_list") {
  if (type == "case_list") {
    # Initialize tracking_list if not already initialized
    if (is.null(.tracking_env$tracking_list)) {
      .tracking_env$tracking_list <- data.frame(
        case_id = character(),
        status = character(),
        stringsAsFactors = FALSE
      )
    }

    # Ensure cases is a character vector
    if (!is.character(cases)) {
      stop("For 'case_list', cases should be a character vector of case IDs.")
    }

    # Check for cases that are already in the tracking list to avoid duplicates
    existing_cases <- .tracking_env$tracking_list$case_id
    new_cases <- cases[!cases %in% existing_cases]

    # Add only new cases to tracking list with the user-defined initial status
    if (length(new_cases) > 0) {
      new_entries <- data.frame(
        case_id = new_cases,
        status = status,
        stringsAsFactors = FALSE
      )
      .tracking_env$tracking_list <- rbind(.tracking_env$tracking_list, new_entries)
    }
  } else if (type == "case_count") {
    # Initialize tracking_count if not already initialized
    if (is.null(.tracking_env$tracking_count)) {
      .tracking_env$tracking_count <- data.frame(
        case_count = integer(),
        status = character(),
        stringsAsFactors = FALSE
      )
    }

    # Ensure cases is an integer for case_count
    if (!is.numeric(cases) || length(cases) != 1) {
      stop("For 'case_count', cases should be a single integer representing the count.")
    }

    # Add the initial count to tracking count with the user-defined status
    new_entry <- data.frame(
      case_count = cases,
      status = status,
      stringsAsFactors = FALSE
    )
    .tracking_env$tracking_count <- rbind(.tracking_env$tracking_count, new_entry)
  } else {
    stop("Invalid type specified. Use 'case_list' or 'case_count'.")
  }
}
