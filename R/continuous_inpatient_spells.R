#' Continuous Inpatient (CIP) Spells
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Creates **Continuous Inpatient (CIP) spells** by combining one or more
#' provider spells into a single uninterrupted period of inpatient care.
#' CIP definitions follow the NHS Digital methodology: transfers between
#' providers can be part of the same CIP spell where specific admission,
#' discharge, and timing criteria are met. A CIP spell begins when a patient is
#' admitted under consultant care and ends when they are discharged or die.
#' http://content.digital.nhs.uk/media/11859/Provider-Spells-Methodology/pdf/Spells_Methodology.pdf
#'
#' Where spells meet the CIP criteria, they are merged into a continuous spell.
#' The output includes a CIP index and the derived start and end dates for
#' the full CIP period.
#'
#' @import data.table
#'
#' @param x A data.frame or data.table; will be converted to a data.table (usually HES/SUS data)
#' @param group_vars Character vector of variables used to group records
#'   (minimum: a patient identifier).
#' @param spell_start_date Quoted column name containing the provider spell
#'   admission date.
#' @param admission_method CDS admission method code.
#' @param admission_source CDS admission source code.
#' @param spell_end_date Quoted column name containing the provider spell
#'   discharge date.
#' @param discharge_destination CDS discharge destination code.
#' @param patient_classification CDS patient classification code.
#' @param .forceCopy Logical (default `FALSE`).
#'   If `FALSE`, the input is converted to a `data.table` and modified by
#'   reference.
#'   If `TRUE`, the input must already be a `data.table`, and the function will
#'   create an explicit copy to avoid modifying the original object.
#'
#' @return
#' A `data.table` containing the original data and three new variables:
#' \describe{
#'   \item{`cip_indx`}{Unique identifier for the derived CIP spell.}
#'   \item{`cip_spell_start`}{Start date for the continuous inpatient spell.}
#'   \item{`cip_spell_end`}{End date for the continuous inpatient spell.}
#' }
#'
#' @examples
#' cip_test <- data.frame(
#'   id = c('465','465','465','465','8418','8418','8418',
#'          '8418','8418','8418','8418','8418','26443',
#'          '26443','26443','33299','33299','33299','33299',
#'          '33299','33299','33299','33299','33299','33299',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','78915','78915','78915'),
#'   provider = c('X1T','X1T','X1T','X1T','KHA','KHA','KHA',
#'                'KHA','KHA','KHA','KHA','KHA','BX2','BX2',
#'                'BX2','PXH','PXH','PXH','PXH','PXH','PXH',
#'                'PXH','PXH','PXH','PXH','9HA','9HA','9HA',
#'                '9HA','9HA','9HA','9HA','9HA','9HA','9HA',
#'                '9HA','9HA','9HA','9HA','9HA','9HA','YYT',
#'                'YYT','YYT','YYT','YYT','YYT','YYT','YYT',
#'                'YYT','YYT','YYT','ABX','ABX','ABX'),
#'   spell_start = as.Date(c(
#'     '2020-03-07','2020-03-07','2020-03-25','2020-04-03','2020-01-25',
#'     '2020-01-26','2020-07-14','2020-08-02','2020-08-12','2020-08-19',
#'     '2020-08-19','2020-11-19','2019-11-12','2020-04-17','2020-04-23',
#'     '2020-07-03','2020-01-17','2020-02-07','2020-03-20','2020-04-27',
#'     '2020-06-21','2020-07-02','2020-10-17','2020-11-27','2021-01-02',
#'     '2019-12-31','2020-01-02','2020-01-14','2020-01-16','2020-02-07',
#'     '2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25',
#'     '2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13',
#'     '2020-03-14','2020-02-04','2020-02-07','2020-02-11','2020-02-14',
#'     '2020-02-18','2020-02-21','2020-02-25','2020-02-28','2020-03-09',
#'     '2020-03-11','2020-03-12','2020-04-16','2020-04-24','2020-05-13')),
#'   spell_end = as.Date(c(
#'     '2020-03-07','2020-03-25','2020-04-02','2020-04-27','2020-01-25',
#'     '2020-01-27','2020-07-17','2020-08-07','2020-08-14','2020-08-19',
#'     '2020-08-22','2020-12-16','2020-04-17','2020-04-23','2020-05-20',
#'     '2020-07-24','2020-01-28','2020-02-07','2020-03-23','2020-04-29',
#'     '2020-06-21','2020-07-03','2020-11-27','2021-01-02','2021-01-10',
#'     '2019-12-31','2020-01-11','2020-01-14','2020-02-04','2020-02-07',
#'     '2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25',
#'     '2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13',
#'     '2020-03-30','2020-02-07','2020-02-11','2020-02-14','2020-02-18',
#'     '2020-02-21','2020-02-25','2020-02-28','2020-03-09','2020-03-11',
#'     '2020-03-12','2020-03-13','2020-04-24','2020-05-13','2020-06-11')),
#'   adm_meth = c('21','81','21','81','21','21','11','21','21','21','21',
#'                '21','21','81','21','81','21','21','21','21','21','21',
#'                '21','13','13','12','22','12','2D','13','13','13','13',
#'                '13','13','13','13','13','13','13','21','81','81','81',
#'                '81','81','13','81','81','13','13','13','21','11','81'),
#'   adm_src = c('19','51','19','51','19','51','19','51','19','19','19',
#'               '51','19','51','19','51','19','19','19','19','19','19',
#'               '19','51','19','19','19','19','19','19','19','19','19',
#'               '19','19','19','51','51','51','51','19','51','51','51',
#'               '51','51','51','51','51','51','51','51','19','51','51'),
#'   dis_meth = c('1','1','1','1','1','1','1','1','1','1','1','4','1','1',
#'                '4','1','1','1','1','1','1','1','8','1','4','1','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','1','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','1','2'),
#'   dis_dest = c('51','51','51','54','51','19','19','19','19','51','19',
#'                '79','51','51','79','65','19','19','19','19','19','29',
#'                '98','51','79','19','19','19','51','19','19','19','51',
#'                '51','51','19','19','51','51','19','51','51','51','51',
#'                '51','51','51','51','51','51','51','51','29','54','19'),
#'   patclass = c('1','1','1','1','1','1','1','1','1','1','1','1','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','2','1','2',
#'                '1','2','2','2','2','2','2','2','2','2','2','2','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','1','1')
#' )
#'
#' cip_spells(x=cip_test,
#'   group_vars = c('id','provider'),
#'   patient_classification = 'patclass',
#'   spell_start_date = 'spell_start',
#'   admission_method = 'adm_meth',
#'   admission_source = 'adm_src',
#'   spell_end_date = 'spell_end',
#'   discharge_destination = 'dis_dest'
#' )[]
#' @export
#'

cip_spells <- function(x,
                       group_vars,
                       spell_start_date,
                       admission_method,
                       admission_source,
                       spell_end_date,
                       discharge_destination,
                       patient_classification,
                       .forceCopy = FALSE) {


  if (!is.data.frame(x) && !data.table::is.data.table(x)) {
    stop("Input `x` must be a data.frame or data.table.")
  }

  # Error handling

  # Validate input columns
  required_cols <- c(group_vars, spell_start_date, admission_method, admission_source,
                     spell_end_date, discharge_destination, patient_classification)

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
    stop("Input data has zero rows. Cannot compute CIP spells.")
  }

  # Validate grouping variables
  if (anyDuplicated(group_vars)) {
    stop("`group_vars` contains duplicates. Provide unique column names.")
  }

  # Warning about missing dates
  if (anyNA(x[[spell_start_date]]) || anyNA(x[[spell_end_date]])) {
    warning("There are missing values in date columns. Results may be affected.")
  }

  ## convert data.frame to data.table or take a copy
  if (.forceCopy && !data.table::is.data.table(x)) {
    stop(force_copy_error)
  }

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## just arrange the data
  data.table::setorderv(x,c(eval(group_vars),spell_start_date))

  ## counter columns to make life easier
  x[,
    tmp.spellN := seq_len(.N),
    by = group_vars
  ]

  ## CIP CRITERIA ##############################################################
  # difference between admission and discharge is <2 days
  x[,
    tmp.cip2daydiff := as.numeric(
      difftime(
        data.table::shift(get(spell_start_date),n=1,type = "lead"),
        get(spell_end_date),
        units="days")
    ) %in% c(0,1,2),
    by = group_vars
  ]

  # a transfer has taken place (based on these criteria)
  # used the simple criteria, as we dont need to determine transfer type (1,2,3)
  x[,
    tmp.cipTransfer :=
      get(discharge_destination) %in%
      c("49", "50", "51", "52", "53", "84") |
      data.table::shift(get(admission_source),n=1,type="lead") %in%
      c("49", "50", "51", "52", "53", "87") |
      data.table::shift(get(admission_method),n=1,type="lead") %in%
      c("2B", "81"),
    by = group_vars
  ]


  # exclusion criteria
  x[,
    tmp.cipExclude :=
      get(discharge_destination) %in% c("19") &
      data.table::shift(get(admission_source),
                        n=1,
                        type="lead") %in% c("51") &
      data.table::shift(get(admission_method),
                        n=1,
                        type="lead") %in% c("21"),
    by = group_vars
  ]

  ## call the other epidm function to clean the dates.
  x <- epidm::proxy_episode_dates(x = x,
                                  group_vars = group_vars,
                                  spell_start_date = spell_start_date,
                                  spell_end_date = spell_end_date,
                                  discharge_destination = discharge_destination,
                                  .dropTmp = FALSE)

  ## setup requirement variables
  x[,tmp.dateNumStart := as.numeric(get(spell_start_date))]
  x[,tmp.dateNumEnd := as.numeric(get(spell_end_date))]
  x[,tmp.regular_attender := as.character(patient_classification) %in% c("3","4")]


  # group records using tmp.cip_valid
  x[,
    tmp.cip_valid :=
      tmp.cip2daydiff &
      tmp.cipTransfer &
      !tmp.cipExclude &
      !tmp.regular_attender
  ]

  x[,
    tmp.cip_valid := data.table::fcase(
      tmp.cip_valid==TRUE, TRUE,
      data.table::shift(tmp.cip_valid,n=1,type="lag"), TRUE,
      is.na(tmp.cip_valid), FALSE,
      default = FALSE
    ),
    by = group_vars
  ]

  ## GROUP UP THE TIME CHUNKS ##################################################
  ## +2 to tmp.windowCmax to allow for up to 2-day window in line with tmp.cip2daydiff

  x[,
    tmp.windowNext := data.table::fifelse(
      tmp.cip_valid,
      data.table::shift(tmp.dateNumStart,
                        n=1,type="lead",
                        fill = data.table::last(tmp.dateNumStart)),
      (tmp.spellN+1)^3
    ),
    by = group_vars
  ]
  x[,
    tmp.windowCmax := data.table::fifelse(
      tmp.cip_valid,
      cummax(tmp.dateNumEnd),
      tmp.spellN),
    by = group_vars
  ]

  x[,
    cip_indx := paste0(
      .GRP,
      ".",
      .N,
      ".",
      c(0,cumsum(tmp.windowNext > tmp.windowCmax))[-.N]
    ),
    by = group_vars
  ]


  x[,
    c('cip_spell_start',
      'cip_spell_end')
    :=
      .(
        min(get(spell_start_date)),
        max(get(spell_end_date))
      ),
    by = cip_indx]


  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}
