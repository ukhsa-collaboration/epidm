#' Lookup table switch handler
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A function to recode values via named lookup tables (i.e call an epidm lookup table and
#' recode where we are aware of a new value). It routes to a specific lookup based on type,
#' returning a character vector where each input value has been mapped to its corresponding
#' replacement. If a value is not found in the lookup then the original value is returned.
#'
#' Built‑in lookups include:
#' - **`species`**: Uses the `respeciate_organism` dataset to standardise and
#'   reclassify organism names (e.g., historic → current nomenclature). This
#'   supports consistent reporting across SGSS and other laboratory datasets.
#'
#' - **`specimen`**: Uses the `specimen_type_grouping` dataset to assign raw
#'   laboratory specimen types into harmonised specimen groups. This enables
#'   consistent grouping for reporting, aggregation, and filtering.
#'
#' - **`genus_gram_stain`**: Uses the `genus_gram_stain` lookup table, which
#'   provides Gram stain classifications by bacterial genus. This reference is
#'   manually maintained against the UKHSA SGSS database and supports rapid
#'   filtering and high‑level organism categorisation. Users should raise an
#'   issue or submit a pull request to the `epidm` GitHub repository if
#'   an organism/genus is missing.
#'
#' - **`lab_data`**: Uses the `lab_data` lookup dataset for harmonising
#'   laboratory code systems and internal SGSS mappings, supporting standardised
#'   laboratory result interpretation within surveillance pipelines.
#'
#' - **`inpatient_admission_method`**: Uses the internal lookup table
#'   `epidm:::group_inpatient_admission_method` to categorise raw hospital
#'   admission method codes into operationally meaningful groups.
#'
#' - **`inpatient_discharge_destination`**: Uses the internal table
#'   `epidm:::group_inpatient_discharge_destination` to group hospital discharge
#'   destination codes into standardised categories for inpatient pathway
#'   analysis.
#'
#' - **`ecds_destination_code`**: Uses the internal table
#'   `epidm:::group_ecds_discharge_destination`, providing grouped mappings for
#'   ECDS (Emergency Care Data Set) discharge codes.
#'
#' - **`manual`**: Allows the user to supply their own lookup through
#'   `.import = list(new, old)`. This is useful when working with local,
#'   provisional, or evolving code sets not yet included in the package’s
#'   centralised lookup tables.
#'
#' @param src Character vector (or column) of values to recode. Coerced to character if needed.
#' @param type Character scalar specifying the lookup to use. One of:
#'   `'species'`, `'specimen'`, `'inpatient_admission_method'`,
#'   `'inpatient_discharge_destination'`, `'ecds_destination_code'`, `'manual'`.
#' @param .import A two‑element list in the format `list(new, old)` used only
#'   when `type = 'manual'`. Each element must be a vector of equal length.
#'
#' @importFrom purrr imap_chr
#' @importFrom stats na.omit setNames
#'
#'
#' @return
#' A character vector containing the recoded values, aligned 1:1 with `src`.
#' Values not present in the lookup are returned unchanged.
#'
#' @examples
#' df <- data.frame(
#'   spec = c(
#'     sample(grep(")",
#'                 respeciate_organism$previous_organism_name,
#'                 value=TRUE,
#'                 invert = TRUE),
#'            9),
#'     "ESCHERICHIA COLI","SARS-COV-2","CANDIDA AUREUS"),
#'   type = sample(specimen_type_grouping$specimen_type,12),
#'   date = sample(seq.Date(from = Sys.Date()-365,
#'                          to = Sys.Date(),
#'                          by = "day"),12)
#' )
#' df <- df[order(df$date),]
#'
#' # show the data before the changes
#' df
#'
#' # check the lookup tables
#' # observe the changes
#' head(respeciate_organism[1:2])
#' df$species <- lookup_recode(df$spec,'species')
#' df[,c('spec','species','date')]
#'
#' head(specimen_type_grouping)
#' df$grp <- lookup_recode(df$type,'specimen')
#' df[,c('species','type','grp','date')]
#'
#' # for a tidyverse use
#' # df %>% mutate(spec=lookup_recode(spec,'species))
#'
#' # manual input of your own lookup
#' # .import=list(new,old)
#' lookup_recode(
#'   "ALCALIGENES DENITRIFICANS",
#'   type = 'manual',
#'   .import=list(respeciate_organism$organism_species_name,
#'                respeciate_organism$previous_organism_name)
#'   )
#'

lookup_recode <- function(src,
                          type=c('species',
                                 'specimen',
                                 'inpatient_admission_method',
                                 'inpatient_discharge_destination',
                                 'ecds_destination_code',
                                 'manual'),
                          .import = NULL) {
  # Error handling
  # src must be provided
  if (missing(src)) {
    stop("'src' must be supplied.", call. = FALSE)
  }

  # Check type of src is correct
  if (!(is.atomic(src) || is.factor(src))) {
    stop("'src' must be a character, numeric, logical, or factor vector (i.e., a column).", call. = FALSE)
  }

  type <- match.arg(type)

  if (type == 'manual' & missing(.import)) {
    stop("supply a two item list for the lookup table in the format list(new,old)")
  }

  if(type == "species"){

    # Verify lookup exists
    if (!exists("respeciate_organism", inherits = TRUE)) {
      stop("Lookup table 'respeciate_organism' not found in the environment.")
    }

    ## calls upon the lookup table stored in the epidm package
    ## data(respeciate_organism)
    lk <- as.list(
      setNames(
        respeciate_organism$organism_species_name,
        respeciate_organism$previous_organism_name
      )
    )

  } else if (type == "specimen") {

    # Verify lookup exists
    if (!exists("specimen_type_grouping", inherits = TRUE)) {
      stop("Lookup table 'specimen_type_grouping' not found in the environment.")
    }

    ## calls upon the lookup table stored in the epidm package
    ## data(specimen_type_grouping)
    lk <- as.list(
      setNames(
        specimen_type_grouping$specimen_group,
        specimen_type_grouping$specimen_type
      )
    )

  } else if (type == "inpatient_admission_method") {

    # Verify lookup exists
    if (!exists("group_inpatient_admission_method", inherits = TRUE)) {
      stop("Lookup table 'group_inpatient_admission_method' not found in the environment.")
    }

    ## calls upon the internal lookup table stored in the epidm package
    ## epidm:::group_inpatient_admission_method
    lk <- as.list(
      setNames(
        group_inpatient_admission_method[[2]],
        group_inpatient_admission_method[[1]]
      )
    )
  } else if (type == "inpatient_discharge_destination") {

    # Verify lookup exists
    if (!exists("group_inpatient_discharge_destination", inherits = TRUE)) {
      stop("Lookup table 'group_inpatient_discharge_destination' not found in the environment.")
    }

    ## calls upon the internal lookup table stored in the epidm package
    ## epidm:::group_inpatient_admission_method
    lk <- as.list(
      setNames(
        group_inpatient_discharge_destination[[2]],
        group_inpatient_discharge_destination[[1]]
      )
    )
  } else if (type == "ecds_destination_code") {

    # Verify lookup exists
    if (!exists("group_ecds_discharge_destination", inherits = TRUE)) {
      stop("Lookup table 'group_ecds_discharge_destination' not found in the environment.")
    }

    ## calls upon the internal lookup table stored in the epidm package
    ## epidm:::group_inpatient_admission_method
    lk <- as.list(
      setNames(
        group_ecds_discharge_destination[[2]],
        group_ecds_discharge_destination[[1]]
      )
    )

  } else if (type == 'manual') {

    # Verify length of each
    if (length(.import[[1]]) != length(.import[[2]])) {
      stop("new and old values of the two item list 'list(new,old)' of .import must be the same length")
    }

    lk <- as.list(
      stats::setNames(
        .import[[1]], ## NEW replacement
        .import[[2]]  ## OLD value
      )
    )

  }


  # create a list of the original and lookup elements
  x <- unname(lk[src])


  ## what happens if the value does not exist in the lookup
  ## use the original value, as they will often be overwritten
  nullReplace <- function(x,y) {
    if(is.null(x)) {
      src[y]
    } else {
      x
    }
  }

  ## purrr::imap uses two arguments, the first is the mapped item
  ## the second is the index number of that item
  ## you do not need to specify the arguments if there are only 2
  ## use _chr to ensure that its reported back as a character vector and not a list
  x <- purrr::imap_chr(x,nullReplace)

  return(x)

}
