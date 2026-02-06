#' Inpatient Codes cleanup
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' When HES/SUS ICD/OPCS codes are provided in wide format
#'   you may want to clean them up into long for easier analysis.
#'   This function helps by reshaping long as a separate table.
#'   Ensuring they're separate allows you to retain source data, and aggregate
#'   appropriately later.
#'
#' @import data.table
#'
#' @param x a data.frame or data.table containing inpatient data
#' @param field_strings a vector or string containing the regex for the the columns
#' @param patient_id_vars a vector containing colnames used to identify a patient episode or spell
#' @param type a string to denote if the codes are diagnostic or procedural
#' @param .forceCopy Logical (default `FALSE`).
#'   If `FALSE`, the input is converted to a `data.table` and modified by
#'   reference.
#'   If `TRUE`, the input must already be a `data.table`, and the function will
#'   create an explicit copy to avoid modifying the original object.
#'
#' @return a separate table with codes and id in long form
#'
#' @examples
#' inpatient_test <- data.frame(
#'   id = c(1053L, 5487L, 8180L),
#'   spell_id = c("dwPDw", "iSpUq", "qpgk5"),
#'   primary_diagnosis_code = c("K602", "U071-", "I501")
#' )
#'
#' inpatient_codes(
#'   x = inpatient_test,
#'   field_strings = "diagnosis",
#'   patient_id_vars = c("id", "spell_id"),
#'   type = "icd10"
#' )
#'
#' inpatient_codes(
#'   x = inpatient_test,
#'   field_strings = c("procedure_code", "procedure_date"),
#'   patient_id_vars = c("id", "spell_id"),
#'   type = "opcs"
#' )
#' @export

inpatient_codes <- function(x,
                            field_strings,
                            patient_id_vars,
                            type = c('icd9','icd10','opcs'),
                            .forceCopy=FALSE) {

  ## convert data.frame to data.table or take a copy
  if (.forceCopy && !data.table::is.data.table(x)) {
    stop(force_copy_error)
  }

  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  # order_n <- NULL

  ## capture the fields of interest
  ## icd just have the codes
  if(type %in% c('icd9','icd10')){
    fields <- grep(field_strings[1],colnames(x),ignore.case=TRUE,value=TRUE)
    sel <- c(patient_id_vars,fields)
  }

  ## opcs have a date and a procedural code
  if(type=='opcs'){
    fields <- grep(field_strings[1],colnames(x),ignore.case=TRUE,value=TRUE)
    dates <- grep(field_strings[2],colnames(x),ignore.case=TRUE,value=TRUE)
    sel <- c(patient_id_vars,fields,dates)
  }


  ## create a vector of the chosen colnames
  ## use the .. syntax to select the items within the vector
  ## reassignment is required to subset the columns
  x <- x[,..sel]

  ## icd10 and opcs are 4digit codes, so clean em up; dont need the subcodes
  x[,
    (fields) := lapply(.SD,function(x) substr(x,1,4)),
    .SDcols = fields
  ]


  if(type %in% c('icd9','icd10')){
    ## reshape the data from wide to long so we can manipulate it better
    x <- data.table::melt(
      data = x,
      id.vars = patient_id_vars,
      measure_vars = fields,
      variable.name = 'order',
      value.name = type,
      na.rm = TRUE,
      variable.factor = FALSE
    )

    ## order the dataset, makes your life easier
    setorderv(x,c(eval(patient_id_vars)))

  x[,
    order_n := seq_len(.N),
    by = c(eval(patient_id_vars))
    ]
  }
  if(type=='opcs'){
    ## needs to separate out the dates and codes for each
    x <- data.table::melt(
      data = x,
      id.vars = patient_id_vars,
      measure = list(fields,dates),
      variable.name = 'order_n',
      value.name = c(type,'date'),
      na.rm = TRUE,
      variable.factor = FALSE
    )

    ## order the dataset, makes your life easier
    setorderv(x,c(eval(patient_id_vars),'date','order_n'))
  }

  ## drop duplicates
  x <- unique(x,
              by = c(eval(patient_id_vars),type))

  return(x)

}
