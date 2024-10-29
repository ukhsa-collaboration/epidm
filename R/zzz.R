# setup-tracking-env.R
.tracking_env <- new.env(parent = emptyenv())
.tracking_env$tracking_list <- NULL  # Tracks individual case IDs and statuses
.tracking_env$tracking_count <- NULL  # Tracks aggregate case counts
