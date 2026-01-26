#' @title Grouping of intervals or events that occur close together in time
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A utility function to group together observations that represent
#' **overlapping date intervals** (e.g., hospital admission spells) or
#' **events occurring within a defined time window** e.g., specimen dates
#' grouped into infection episodes. The function supports both:
#'
#' • **Interval-based grouping**: records have a start and an end date; any
#'   overlapping intervals are grouped together.
#' • **Event-based grouping**: records have only a start date and are grouped
#'   using either a *static* or *rolling* time window.
#'
#' The output provides a unique index per group and the minimum/maximum date that
#' define the resulting aggregated episode or interval.
#'
#' @details
#' ## How the function works
#'
#' The behaviour depends on whether `date_end` is supplied:
#'
#' ### 1. **Interval-based grouping (start + end dates)**
#' If both `date_start` and `date_end` are provided, the function identifies
#' overlapping intervals within the same `group_vars` grouping. Any intervals
#' that overlap are combined into a single episode.
#'
#' This method is typically used for:
#' - Hospital spells (HES/SUS)
#' - Contact periods or inpatient stays
#'
#' ### 2. **Event-based grouping (single-date events)**
#' If only `date_start` is supplied, records are grouped using a **time window**
#' defined by the `window` argument.
#'
#' Two approaches are supported:
#'
#' - **`window_type = "static"`**
#'   A fixed window is applied starting from the first event in the group. All
#'   events occurring within the window are grouped until a gap exceeds the
#'   threshold, at which point a new episode begins.
#'
#' - **`window_type = "rolling"`**
#'   A dynamic window where each event extends the episode end point. An event
#'   is grouped as long as it occurs within `window` days of the most recent
#'   event in the same episode.
#'
#' ## Handling of missing values
#' Records missing `date_start` cannot be grouped and are returned with `indx`
#' = `NA`. These rows are appended back to the final output.
#'
#' @section Workflow context: how `group_time()` might be used in a pipeline
#' **1) SGSS specimen data – infection episode grouping (event-based)**
#' After organism/specimen harmonisation (e.g., via `lookup_recode()`),
#' `group_time()` groups specimen dates into infection episodes using a defined
#' time window. This helps identify clusters of related positive tests for the
#' same patient and organism.
#'
#' **2) HES/SUS inpatient data – continuous spell grouping (interval-based)**
#' When start and end dates of inpatient stays are available, `group_time()`
#' collapses overlapping intervals into a single continuous hospital spell.
#' This is used before linking SGSS infection episodes to inpatient activity.
#'
#' **3) Integration across datasets**
#' The outputs from `group_time()` are used downstream to determine whether
#' infection events fall within or around periods of hospital care,
#' enabling combined SGSS–HES/SUS–ECDS analyses.
#'
#'
#' @import data.table
#'
#' @param x data frame, this will be converted to a data.table
#' @param group_vars in a vector, the all columns used to group records, quoted
#' @param date_start column containing the start dates for the grouping,
#'   provided quoted
#
#' @param x A data.frame or data.table containing date variables for grouping.
#'   Will be converted to a data.table internally.
#' @param group_vars Character vector of quoted column names used to partition
#'   the data before grouping.
#' @param date_start Quoted column name giving the start date for each record.
#' @param date_end Optional quoted column name giving the end date for interval
#'   records. When omitted, event-based grouping is used.
#' @param window Integer (days) defining the grouping threshold for event-based
#'   methods. Required when `date_end` is missing.
#' @param window_type Character: `"static"` or `"rolling"`. Determines how
#'   events are grouped when only a start date is present.
#' @param indx_varname Name of the output column containing the episode or
#'   interval index. Default: `"indx"`.
#' @param min_varname Name of the output column containing the minimum date
#'   in each grouped episode. Default: `"date_min"`.
#' @param max_varname Name of the output column containing the maximum date
#'   in each grouped episode. Default: `"date_max"`.
#' @param .forceCopy Logical (default `FALSE`).
#'   If `FALSE`, the input is converted to a `data.table` and modified by
#'   reference.
#'   If `TRUE`, the input must already be a `data.table`, and the function will
#'   create an explicit copy to avoid modifying the original object.
#'
#' @return
#' A `data.table` containing all original columns plus:
#' @param date_end column containing the end dates for the *interval*, quoted
#'
#' @param window an integer representing a time window in days which will be
#'   applied to the start date for grouping *events*
#' @param window_type character, to determine if a 'rolling' or 'static'
#'   grouping method should be used when grouping *events*.
#'   A *'static'* window will identify the first event, and all records X days
#'   from that event will be attributed to the same episode. Eg. in a 14 day
#'   window, if first event is on 01 Mar, and events on day 7 Mar and 14 Mar will be
#'   grouped, but an event starting 15 Mar days after will start a new episode.
#'   A *'rolling'* window resets the day counter with each new event. Eg.
#'   Events on 01 Mar, 07 Mar, 14 Mar and 15 Mar are all included in a single episode,
#'   as will any additional events up until the 29 Mar (assuming a 14-day window).
#'
#' @param indx_varname a character string to set variable name for the
#'   index column which provides a grouping key; default is indx
#' @param min_varname a character string to set variable name for the
#'   time period minimum
#' @param max_varname a character string set variable name for the time
#'   period maximum
#' @param .forceCopy default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
#'
#' \describe{
#'   \item{`indx`; renamed using `indx_varname`}{an id field for the new
#'     aggregated events/intervals; note that where the `date_start` is NA, an
#'     `indx` value will also be NA}
#'   \item{`min_date`; renamed using `min_varname`}{the start date for the
#'     aggregated events/intervals}
#'   \item{`max_date`; renamed using `max_varname`}{the end date for the
#'     aggregated events/intervals}
#'   }
#'
#' @examples
#' episode_test <- structure(
#'   list(
#'     pat_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L,
#'                1L, 1L, 1L, 1L, 2L, 2L, 2L),
#'     species = c(rep("E. coli",7),rep("K. pneumonia",7)),
#'     spec_type = c(rep("Blood",7),rep("Blood",4),rep("Sputum",3)),
#'     sp_date = structure(c(18262, 18263, 18281, 18282, 18262, 18263, 18281,
#'                           18265, 18270, 18281, 18283, 18259, 18260, 18281),
#'                         class = "Date")
#'   ),
#'   row.names = c(NA, -14L), class = "data.frame")
#'
#' group_time(x=episode_test,
#'            date_start='sp_date',
#'            window=14,
#'            window_type = 'static',
#'            indx_varname = 'static_indx',
#'            group_vars=c('pat_id','species','spec_type'))[]
#'
#' spell_test <- data.frame(
#'   id = c(rep(99,6),rep(88,4),rep(3,3)),
#'   provider = c("YXZ",rep("ZXY",5),rep("XYZ",4),rep("YZX",3)),
#'   spell_start = as.Date(
#'     c(
#'       "2020-03-01",
#'       "2020-07-07",
#'       "2020-02-08",
#'       "2020-04-28",
#'       "2020-03-15",
#'       "2020-07-01",
#'       "2020-01-01",
#'       "2020-01-12",
#'       "2019-12-25",
#'       "2020-03-28",
#'       "2020-01-01",
#'       rep(NA,2)
#'     )
#'   ),
#'   spell_end = as.Date(
#'     c(
#'       "2020-03-10",
#'       "2020-07-26",
#'       "2020-05-22",
#'       "2020-04-30",
#'       "2020-05-20",
#'       "2020-07-08",
#'       "2020-01-23",
#'       "2020-03-30",
#'       "2020-01-02",
#'       "2020-04-20",
#'       "2020-01-01",
#'       rep(NA,2)
#'     )
#'   )
#' )
#'
#' group_time(x = spell_test,
#'            date_start = 'spell_start',
#'            date_end = 'spell_end',
#'            group_vars = c('id','provider'),
#'            indx_varname = 'spell_id',
#'            min_varname = 'spell_min_date',
#'            max_varname = 'spell_max_date')[]
#' @export


group_time <- function(x,
                       date_start,
                       date_end,
                       window,
                       window_type = c('rolling','static'),
                       group_vars,
                       indx_varname = 'indx',
                       min_varname = 'date_min',
                       max_varname = 'date_max',
                       .forceCopy = FALSE
){


  if (!is.data.frame(x) && !data.table::is.data.table(x)) {
    stop("'x' must be a data.frame or data.table")
  }

  if (!date_start %in% names(x)) stop(paste("Column", date_start, "not found in x"))

  if (!missing(date_end) && !date_end %in% names(x)) stop(paste("Column", date_end, "not found in x"))

  if (!all(group_vars %in% names(x))) stop("Some group_vars not found in x")

  if (!is.numeric(window) || window <= 0) stop("'window' must be a positive numeric value")

  if (!window_type %in% c("rolling", "static")) stop("'window_type' must be 'rolling' or 'static'")

  if (!inherits(x[[date_start]], "Date")) stop(paste(date_start, "must be of class Date"))

  if (sum(!is.na(x[[date_start]])) == 0) {
    warning("No valid rows with non-NA start dates; returning original data.")
    return(x)
  }

  ## convert data.frame to data.table or take a copy
  if (.forceCopy && !data.table::is.data.table(data)) {
    stop(force_copy_error)
  }

  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  # setup NSE
  # subtitute() not needed on other vars as quoted so use get()
  group_vars <- substitute(group_vars)

  ## checks
  if(missing(x)){
    stop("x must be supplied as a data.frame or data.table")
  }
  if(missing(date_start)){
    stop("date_start must be supplied as a quoted column name from x")
  }

  ## seperate out the two halfs if there are missing events/intervals
  ## records in y will not be assigned an indx since there is no event
  y <- x[is.na(get(date_start)), ]
  x <- x[!is.na(get(date_start)), ]


  ## static + window methods only ##############################################
  ## bring the static window function in so its all a one stop shop for ease
  if(missing(date_end)){

    if(missing(window_type)){
      stop("window_type must be specified as either rolling or static")
    }

    if(missing(window)){
      stop("window parameter must be supplied as numeric value, the unit is days")
    }

    ## this is for static grouping of single date windows
    if(window_type=='static'){

      ## set a fixed ordering
      data.table::setorderv(x, c(date_start))

      ## move through static episodes in a loop.
      ## next steps: update with data.table::set()
      x[,tmp.episode:=0]

      ## setup loop episode counter
      i <- 1L

      while (min(x[['tmp.episode']]) == 0) {

        x[tmp.episode==0,
          c('tmp.diff',
            'tmp.diff.start'
          ) := .(
            as.integer(difftime(get(date_start),
                                data.table::shift(date_start,n=1,type="lag"),
                                units="days")),
            as.integer(difftime(get(date_start),
                                get(date_start)[1],
                                units="days"))
          ),
          by = group_vars
        ][tmp.episode==0,
          tmp.diff := data.table::fifelse(is.na(tmp.diff),0L,tmp.diff),
          by = group_vars
        ][tmp.episode == 0,
          tmp.episode := data.table::fifelse(tmp.diff <= window &
                                               tmp.diff.start <= window,
                                             i,
                                             tmp.episode),
          by = group_vars
        ]

        i <- i + 1L
      } # static loop

      x[,
        indx := paste0(
          .GRP,
          ".",
          .N,
          ".",
          tmp.episode),
        keyby = group_vars]

      ## only produce these if the arguments are defined
      if(!missing(min_varname) & !missing(max_varname))
        x[,
          c('min_date',
            'max_date'
          ) := .(
            min(get(date_start)),
            max(get(date_start))
          ),
          by = indx
        ]


    } # static method

    if(window_type == 'rolling') {
      x[,tmp.windowEnd := as.numeric(get(date_start)) + window]
    }
  }

  ## everything else ###########################################################
  if(any(window_type == "rolling" | !missing(date_end))) {

    ## change the dates into numeric
    x[,tmp.dateNum := as.numeric(get(date_start))]

    if(!missing(date_end)){
      x[,tmp.windowEnd := as.numeric(get(date_end))]
    }

    ## set sort order
    data.table::setorder(x,tmp.dateNum)

    ## look at the next start date
    x[,
      tmp.windowStart := data.table::shift(
        tmp.dateNum,
        1,
        type="lead",
        fill = tmp.dateNum[.N]
      ),
      by = group_vars
    ]

    ## compare the end end date within the groups
    x[,
      tmp.windowCmax := cummax(tmp.windowEnd),
      by = group_vars
    ]

    ## correct for missing values
    x[,
      tmp.windowCmax := data.table::fifelse(
        is.na(tmp.windowCmax) & !is.na(tmp.windowEnd),
        tmp.windowEnd,
        tmp.windowCmax
      ),
      by = group_vars
    ]

    ## create an index to group records sequentially and overlapping in time
    x[,
      indx := paste0(
        .GRP,
        ".",
        .N,
        ".",
        c(0,
          data.table::fifelse(
            is.na(tmp.dateNum),
            .I,
            cumsum(tmp.windowStart > tmp.windowCmax)
          )[-.N]
        )
      ),
      by = group_vars
    ]

    ## create new columns back in date format
    x[,
      min_date := min(as.Date(tmp.dateNum, origin="1970-01-01")),
      by = indx
    ]

    if(!missing(date_end)){
      ## there are confirmed end dates, so use them
      x[,
        max_date := max(as.Date(tmp.windowCmax, origin="1970-01-01")),
        by = indx
      ]
    } else {
      ## these are for windows, so we cant always assume the window
      ## time was still relevant, so we use the last known time in the series
      x[,
        max_date := min(as.Date(tmp.windowCmax, origin="1970-01-01")),
        by = indx
      ]
    }

  } ## rolling windows


  ## variable cleanup ##########################################################
  ## rename if arguments are provided
  if(min_varname!="min_date" & !missing(min_varname)){
    data.table::setnames(x,'min_date',min_varname)
  }
  if(max_varname!="max_date" & !missing(max_varname)){
    data.table::setnames(x,'max_date',max_varname)
  }

  ## allow rename of indx column for multiple runs
  if(indx_varname!='indx' & !missing(indx_varname)){
    data.table::setnames(x,'indx',indx_varname)
  }

  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  ## rejoin on the missing section
  x <- data.table::rbindlist(
    list(x,y),
    fill = TRUE
  )

  return(x)

}
