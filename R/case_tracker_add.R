#' Add cases to the tracking list or initialize case count
#'
#' Adds cases to the tracking list with an initial status of "active", or
#' initializes the case count with a specified count value at the start
#' of the pipeline.
#'
#' @param cases A character vector of unique case IDs if `type = "case_list"`,
#'   or an integer case count if `type = "case_count"`.
#' @param type Character. Indicates whether to update `"case_list"` (default)
#'   or `"case_count"`.
#'
#' @return None. Updates the tracking list or initializes the case count.
#' @export
case_tracker_add <- function(cases, type = "case_list") {
  if (type == "case_list") {
    # Initialize tracking_list if not already initialized
    if (is.null(.tracking_env$tracking_list)) {
      .tracking_env$tracking_list <- data.frame(case_id = character(),
                                                status = character(),
                                                stringsAsFactors = FALSE)
    }

    # Ensure cases is a character vector
    if (!is.character(cases)) {
      stop("For 'case_list', cases should be a character vector of case IDs.")
    }

    # Add cases to tracking list with status "active"
    new_entries <- data.frame(case_id = cases, status = "active",
                              stringsAsFactors = FALSE)
    .tracking_env$tracking_list <- rbind(.tracking_env$tracking_list,
                                         new_entries)
  } else if (type == "case_count") {
    # Initialize tracking_count if not already initialized
    if (is.null(.tracking_env$tracking_count)) {
      .tracking_env$tracking_count <- data.frame(case_count = integer(),
                                                 status = character(),
                                                 stringsAsFactors = FALSE)
    }

    # Ensure cases is an integer for case_count
    if (!is.numeric(cases) || length(cases) != 1) {
      stop("For 'case_count', cases should be a single integer representing the count.")
    }

    # Add the initial count to tracking count with status "initial import"
    new_entry <- data.frame(case_count = cases, status = "initial import",
                            stringsAsFactors = FALSE)
    .tracking_env$tracking_count <- rbind(.tracking_env$tracking_count,
                                          new_entry)
  } else {
    stop("Invalid type specified. Use 'case_list' or 'case_count'.")
  }
}
