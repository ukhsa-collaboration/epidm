#' Connect to a SQL database
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' An function to help setup connections to SQL databases
#'   acting as a wrapper for the odbc and DBI packages. Used by other sql_*
#'   tools within epidm. This uses the credential manager within the system and
#'   assumes you are using a trusted connection.
#'
#' @importFrom odbc odbcListDrivers dbConnect odbc
#' @importFrom DBI dbIsValid
#'
#' @param server a string containing the server connection;
#'   note that servers may require the use of double backslash `\\`
#' @param database a string containing the database name within the data store
#'
#' @seealso sql_clean sql_read sql_write
#'
#' @return a SQL connection object
#'
#' @examples
#' \dontrun{
#' sql <- list(
#'   dsn = list(ser = 'covid.ukhsa.gov.uk',
#'              dbn = 'infections')
#' )
#'
#' sgss_con = sql_connect(server = sql$dsn$ser, database = sql$dsn$dbn)
#' }
#'
#' @export
#'

sql_connect <- function(server,
                        database){

  # # get correct driver for ODBC connection
  # if('SQL Server' %in% unique(odbc::odbcListDrivers()$name)) {
  #   # windows
  #   SQLdriver <- 'SQL Server'
  # } else if('SQL Server' %in% unique(odbc::odbcListDrivers()$name)) {
  #   # linux [Debian-flavours]
  #   SQLdriver <- 'ODBC Driver 17 for SQL Server'
  # } else {
  #   stop("SQL server drivers require installation")
  # }

  ## cycle through the available drivers on the machine to find the right one
  ## this is helpful if you're on a linux system/container and not Windows

  # Error handling

  if (missing(server) || !is.character(server) || is.null(server) || length(server) == 0 || server == "") {
    stop("'server' must be a non-empty character string.")
  }
  if (missing(database) || !is.character(database) || is.null(server) || length(database) == 0 || database == "") {
    stop("'database' must be a non-empty character string.")
  }

  # Check available drivers
  drivers <- unique(odbcListDrivers()$name)
  if (length(drivers) == 0) {
    stop("No ODBC drivers found. Please install SQL Server ODBC drivers.")
  }

  # Message to use '\\' in the R server connection string.
  message("Please note: Use '\\\\' in R strings for server connections.")

  for(driver in unique(odbcListDrivers()$name)){

    ## uses Active Directory credentials
    conString <- paste0('driver={',driver,'};',
                        'server=',server,';',
                        'database=',database,';',
                        'trusted_connection=yes;',
                        'timeout=120'
                        )

    # connect to the database safely
    odbcConnect <- tryCatch({
      dbConnect(
        odbc(),
        .connection_string = conString
      )
    }, error = function(e) NULL)

    # check if connection is valid
    if (!is.null(odbcConnect) && dbIsValid(odbcConnect)) {
      break
    }
  }

  # Final validation
  if (is.null(odbcConnect) || !dbIsValid(odbcConnect)) {
    stop("Failed to establish a SQL connection. Verify server name, database name, and that ODBC drivers are installed.")
  }

  return(odbcConnect)
}
