#' @title Clean and Impute HES/SUS Episode Start and End Dates
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A utility for cleaning and imputing missing or inconsistent episode end dates
#' in HES/SUS–style inpatient data. The function identifies missing, invalid,
#' or overlapping spell dates within patient/provider groups and applies
#' deterministic rules to correct them. It also assigns a flag (`proxy_missing`)
#' indicating whether a value was modified and why.
#'
#' @import data.table
#'
#' @param x A `data.frame` or `data.table`. Will be converted to a
#'   `data.table` if not already.
#' @param group_vars Character vector of grouping variables (e.g., patient ID,
#'   provider). At least one identifier must be supplied.
#' @param spell_start_date Name of the column containing the episode or spell
#'   start date.
#' @param spell_end_date Name of the column containing the episode or spell
#'   end date.
#' @param discharge_destination Name of the column containing the CDS discharge
#'   destination code.
#' @param .dropTmp Logical (default `TRUE`). If `TRUE`, temporary processing
#'   columns are removed before returning the result.
#' @param .forceCopy Logical (default `FALSE`).
#'   If `FALSE`, the input is converted to a `data.table` and modified by
#'   reference.
#'   If `TRUE`, the input must already be a `data.table`, and the function will
#'   create an explicit copy to avoid modifying the original object.
#'
#' @return
#' A `data.table` containing:
#'
#' * Cleaned spell start and end dates.
#' * A flag variable (`proxy_missing`) indicating whether a date was modified
#'   and the rule applied (0–4).
#'
#' @export
#'
#' @examples
#'
#' proxy_test <- data.frame(
#'   id = c(
#'     rep(3051, 4),
#'     rep(7835,3),
#'     rep(9891,3),
#'     rep(1236,3)
#'   ),
#'   provider = c(
#'     rep("QKJ", 4),
#'     rep("JSD",3),
#'     rep("YJG",3),
#'     rep("LJG",3)
#'   ),
#'   spell_start = as.Date(c(
#'     "2020-07-03", "2020-07-14", "2020-07-23", "2020-08-05",
#'     "2020-11-01", "2020-11-13", "2020-12-01",
#'     "2020-03-28", "2020-04-06", "2020-04-09",
#'     "2020-10-06", "2020-11-05", "2020-12-25"
#'   )),
#'   spell_end = as.Date(c(
#'     "2020-07-11", "2020-07-22", "2020-07-30", "2020-07-30",
#'     "2020-11-11", NA, "2020-12-03",
#'     "2020-03-28", NA, "2020-04-09",
#'     "2020-10-06", "2020-11-05", NA
#'   )),
#'   disdest = c(
#'     19, 19, 51, 19,
#'     19, 19, 19,
#'     51, 98, 19,
#'     19, 19, 98
#'   )
#' )
#'
#'
#' proxy_episode_dates(
#'   x=proxy_test,
#'   group_vars = c('id','provider'),
#'   spell_start_date = 'spell_start',
#'   spell_end_date = 'spell_end',
#'   discharge_destination = 'disdest'
#' )[]
#'

proxy_episode_dates <- function(x,
                                group_vars,
                                spell_start_date,
                                spell_end_date,
                                discharge_destination,
                                .dropTmp = TRUE,
                                .forceCopy = FALSE) {


  ## convert data.frame to data.table or take a copy
  if (.forceCopy && !data.table::is.data.table(data)) {
    stop(force_copy_error)
  }

  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  # Error handling

  # Validate input columns
  required_cols <- c(group_vars, spell_start_date, spell_end_date, discharge_destination)

  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Validate date column
  if (!inherits(x[[spell_start_date]], "Date") || !inherits(x[[spell_end_date]], "Date")) {
    stop("Columns for spell start and end dates must be of type Date.")
  }

  # Check for empty data
  if (nrow(x) == 0) {
    stop("Input data has zero rows.")
  }

  # Validate grouping variables
  if (anyDuplicated(group_vars)) {
    stop("`group_vars` contains duplicates. Provide unique column names.")
  }

  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  # proxy_missing <-
  #   tmp.spell.N <- tmp.spell.n <- tmp.spell_start <- tmp.spell_end <-
  #   NULL

  ## just arrange the data
  data.table::setorderv(x,c(eval(group_vars),spell_start_date))

  ## counter columns to make life easier
  x[,
    c('tmp.spell.N',
      'tmp.spell.n',
      'tmp.spell_start',
      'tmp.spell_end'
      ) := .(
        .N,
        seq(1:.N),
        get(spell_start_date),
        get(spell_end_date)
        ),
    by = group_vars
  ]

    ## DATE CLEANUP ##############################################################
  #   check to see if dates are missing, and decide how to replace
  #   0 = unchanged
  #   1 = final episode, no date available, use today
  #   2 = patient still in hospital, so connect to the next episode
  #   3 = patient discharged, so dont connect to the next episode
  #   4 = if the dates overlap, use the start date of the next

  x[,
    proxy_missing := data.table::fcase(
      is.na(get(spell_end_date)) & tmp.spell.n == tmp.spell.N, 1,
      is.na(get(spell_end_date)) & tmp.spell.n < tmp.spell.N &
        get(discharge_destination) %in% c("51","98"), 2,
      is.na(get(spell_end_date)) & tmp.spell.n < tmp.spell.N &
        !get(discharge_destination) %in% c("51","98"), 3,
      !is.na(get(spell_end_date)) &
        get(spell_end_date) < get(spell_start_date), 4,
      default = 0
    )
  ]

  #   replace the dates based on the proxy_missing flag
  #   use the x[,c(var)=.(val)] syntax to allow overwriting of existing col
  #   using a functional argument for the varname
  x[,
    c(spell_end_date,
      spell_start_date
      ) := .(
      data.table::fcase(
        proxy_missing==0, get(spell_end_date),
        proxy_missing==1, Sys.Date(),
        proxy_missing==2, data.table::shift(get(spell_start_date),n=1,type="lead"),
        proxy_missing==3, data.table::shift(get(spell_start_date),n=1,type="lead")-1,
        proxy_missing==4, tmp.spell_start),
      data.table::fifelse(
        proxy_missing==4,
        tmp.spell_end,
        tmp.spell_start
        )
    ),
    by = group_vars
  ]

  ## cleanup and remove temp columns
  if(.dropTmp==TRUE){
    tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
    x[,
      (tmpcols) := NULL
    ]
  }

  return(x)
}



