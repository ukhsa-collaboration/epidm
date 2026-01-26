#'
#' @title Respeciate unspecified samples
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' Some samples within SGSS are submitted by laboratories as "GENUS SP" or
#' "GENUS UNNAMED". However, they may also have a fully identified sample taken
#' from the same site within a recent time period.  This function captures
#' species_col  from another sample within X-days of an unspeciated isolate.
#' Respeciation is restricted to organisms of the same genus; species will not
#' be inferred from isolates belonging to a different genus. Trailing "UNNAMED"
#' is normalised to "SP" before any processing.
#'
#' @import data.table
#' @importFrom stringr str_detect
#'
#' @param x a data.frame or data.table object
#' @param group_vars the minimum grouping set of variables for like samples in
#'   a character vector; suggest c('patient_id','specimen_type') - genus will automatically
#'   be included in the groupby. This is built from the species_col
#' @param species_col a character containing the column with the organism species_col
#'   name
#' @param date_col a character containing the column with the specimen/sample date_col
#' @param window an integer representing the number of days for which you will
#'   allow a sample to be respeciated
#' @param .forceCopy Logical (default `FALSE`).
#'   If `FALSE`, the input is converted to a `data.table` and modified by
#'   reference.
#'   If `TRUE`, the input must already be a `data.table`, and the function will
#'   create an explicit copy to avoid modifying the original object.
#'
#' @return a data.table with a recharacterised `species_col` column
#'
#' @examples
#'
#' df <- data.frame(
#' ptid = c(round(runif(25,1,5))),
#' spec = sample(c("KLEBSIELLA SP",
#'                 "KLEBSIELLA UNNAMED",
#'                 "KLEBSIELLA PNEUMONIAE",
#'                 "KLEBEIELLA OXYTOCA"),
#'               25,replace = TRUE),
#' type = "BLOOD",
#' specdate = sample(seq.Date(Sys.Date()-21,Sys.Date(),"day"),25,replace = TRUE)
#' )
#'
#' respeciate_generic(x=df,
#'                    group_vars=c('ptid','type'),
#'                    species_col='spec',
#'                    date_col='specdate',
#'                    window = 14)[]
#'
#' @export
#'

respeciate_generic <- function(x,
                               group_vars,
                               species_col,
                               date_col,
                               window=c(0:Inf),
                               .forceCopy = FALSE
                               ) {

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
  required_cols <- c(group_vars, species_col, date_col)

  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Validate date column
  if (!inherits(x[[date_col]], "Date")) {
    stop("Column for date_col must be of type Date.")
  }

  # Check for empty data
  if (nrow(x) == 0) {
    stop("Input data has zero rows.")
  }

  # Validate grouping variables
  if (anyDuplicated(group_vars)) {
    stop("`group_vars` contains duplicates. Provide unique column names.")
  }

  # Warning about missing dates
  if (anyNA(x[[date_col]])) {
    warning("There are missing values in date column. Results may be affected.")
  }

  # Normalising species names
  x[, (species_col) := trimws(as.character(get(species_col)))] # Remove any whitespace
  x[, (species_col) := toupper(get(species_col))]                     # upper-case for consistent matching
  x[, (species_col) := sub("\\bUNNAMED\\b$", "SP", get(species_col))] # change UNNAMED -> SP

  ## setup some helpful vars first
  x[,
    c(
      'tmp.genus',
      'tmp.spFlag'
      ) := .(
        gsub("([A-Za-z]+).*", "\\1", get(species_col)),
        data.table::fifelse(
          stringr::str_detect(get(species_col), " SP$|species_col$"),
          1,
          0)
      )
    ]

  ## NSE for group_vars
  group_vars <- eval(substitute(group_vars))

  ## Adding tmp.genus to group vars so no cross genus contamination
  group_vars <- c(group_vars, "tmp.genus")

  ## set a static key and order for the table
  data.table::setorderv(x, c(eval(group_vars),date_col))


  ## loop counters
  i <- 1
  respecCount <- sum(x$tmp.spFlag,na.rm=TRUE)

  while(respecCount!=0) {

    ## Save respecs to check if we are in infinite loop
    if(i > 1){
      xCheck <- sum(x$tmp.respecType %in% c(1,2),na.rm=T)
    }

    ## check for unspeciated samples X days of either side

    ## calculate days and if it needs to be amended
    x[,
      c(
        'tmp.dayR',
        'tmp.spFlag'
      ) := .(
        as.numeric(difftime(
          get(date_col),
          data.table::shift(get(date_col),type="lag"),
          units="days")
          ),
        data.table::fifelse(
          stringr::str_detect(get(species_col),
                              " SP$|species_col$"),
          1,0)

      ),
      by = group_vars
    ][,
      tmp.dayR := data.table::fifelse(is.na(tmp.dayR),0,tmp.dayR)
      ]

    ## does the speciated sample come from before or after
    x[tmp.dayR %in% c(0:window),
      tmp.respecType := data.table::fcase(
        tmp.spFlag==1 & data.table::shift(tmp.spFlag,type="lag")==0, 1,
        tmp.spFlag==1 & data.table::shift(tmp.spFlag,type="lead")==0,2,
        default = 0
      ),
      by = group_vars
    ]

    ## okay; recode the organism based on the flags
    x[,
      c(species_col) := .(
        data.table::fcase(
          tmp.spFlag==0,  get(species_col),
          tmp.spFlag==1 & tmp.respecType==1, data.table::shift(get(species_col),type="lag"),
          tmp.spFlag==1 & tmp.respecType==2, data.table::shift(get(species_col),type="lead"),
          default = get(species_col)[1]
        )
      ),
      by = group_vars
    ]

    respecCount <- sum(x$tmp.respecType %in% c(1,2),na.rm=TRUE)

    message(paste0(window,
                 "-day round ",i,": ",respecCount,
                 " SP or UNNAMMED isolates respeciated"))

    ## Check previous respec vs current
    if(i > 1){
      if(all.equal(xCheck,respecCount)==TRUE){
        respecCount <- 0
        break
      }
    }

    ## next loop
    i <- i + 1

  }

  ## remove the keys
  data.table::setkey(x, NULL)

  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}


