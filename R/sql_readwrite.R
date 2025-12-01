#' Read a table from a SQL database
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' Read a table object to a SQL database. Acts a wrapper for odbc and DBI
#'   packages.
#'
#' @importFrom DBI dbIsValid dbGetQuery dbDisconnect
#' @importFrom odbc dbConnect odbc
#'
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#' @param sql a string containing a SQL query or to a .sql/.txt SQL query
#'
#' @seealso sql_clean sql_connect
#'
#' @return a table from a SQL database
#' @export
#'
sql_read <- function(server,
                     database,
                     sql){

  if (missing(server) || !is.character(server) || is.null(server) || length(server) == 0 || server == "") {
    stop("Parameter 'server' must be a non-empty string.")
  }
  if (missing(database) || !is.character(database) || is.null(database) || length(database) == 0 || database == "") {
    stop("'database' must be a non-empty character string.")
  }
  if (missing(sql) || !is.character(sql) || is.null(sql) || length(sql) == 0 || sql == "") {
    stop("Parameter 'sql' must be a non-empty string containing a query or file path.")
  }

  odbcConnect <- epidm::sql_connect(server = server, database = database)
  on.exit(DBI::dbDisconnect(odbcConnect), add = TRUE)

  if (!DBI::dbIsValid(odbcConnect)) stop("Invalid database connection.")

  query <- epidm::sql_clean(sql)
  timeStart <- Sys.time()

  tableResult <- tryCatch({
    DBI::dbGetQuery(odbcConnect, query)
  }, error = function(e) {
    stop("SQL query failed: ", e$message)
  })

  ## success!
  message(paste0('Data imported in ',
               round(difftime(Sys.time(),
                              timeStart,
                              units = 'mins')),
               'min')
          )

  return(tableResult)

  # close the connection
  DBI::dbDisconnect(odbcConnect)

}



#' Write a table to a SQL database
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' Write a table object to a SQL database. Acts a wrapper for odbc and DBI
#'   packages with additional checks to ensure upload completes.
#'
#' @importFrom DBI dbExistsTable dbGetQuery dbWriteTable dbDisconnect
#' @importFrom odbc dbConnect odbc
#'
#' @param x a data.frame/data.table/tibble object
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#' @param tablename a string containing the chosen SQL database table name
#'
#' @return writes a data.frame/data.table/tibble to a SQL database
#' @export
#'
sql_write <- function(x,
                      server,
                      database,
                      tablename){


  # Validate parameters
  if (missing(x) || !is.data.frame(x) || is.null(x)) {
    stop("Parameter 'x' must be a non-null data.frame.")
  }
  if (nrow(x) == 0) {
    stop("Parameter 'x' must contain at least one row.")
  }
  if (missing(server) || !is.character(server) || is.null(server) || length(server) == 0 || server == "") {
    stop("Parameter 'server' must be a non-empty string.")
  }
  if (missing(database) || !is.character(database) || is.null(database) || length(database) == 0 || database == "") {
    stop("'database' must be a non-empty character string.")
  }
  if (missing(tablename) || !is.character(tablename) || is.null(tablename) || length(tablename) == 0 || tablename == "") {
    stop("Parameter 'tablename' must be a non-empty string.")
  }

  ## connect to the database
  odbcConnect <- epidm::sql_connect(server = server, database = database)
  on.exit(DBI::dbDisconnect(odbcConnect), add = TRUE)

  # used to check if the table outputs upload
  checkSQL <- paste0('SELECT COUNT(*) FROM ',database,'.dbo.',tablename)

  # check the object exists
  if(exists('x')){
    if(nrow(x)>0){
      if(DBI::dbIsValid(odbcConnect)){
        message('connection established')
      } else{
        odbcConnect <- epidm::sql_connect(server = server, database = database)
      }

      ## upload check to ensure the full dataset is uploaded
      if(DBI::dbExistsTable(odbcConnect,tablename)){
        DBrows <- DBI::dbGetQuery(odbcConnect,checkSQL)[1,1]
        message(paste0(DBrows,' records in [',
                       database,'].[dbo].[',tablename,'] currently'))
      } else {
        DBrows <- 0
        message(paste0('[',database,'].[dbo].[',tablename,
                       '] does not exist; creating table.'))
      }

      timeStart <- Sys.time()

      ## this will ensure that object matches the upload
      while(DBrows != nrow(x)){

        message(paste('Start data upload',timeStart))

        DBI::dbWriteTable(conn = odbcConnect,
                          name = DBI::Id(schema = 'dbo',
                                         table   = tablename),
                          value = x,
                          encoding = 'latin1',
                          row.names = FALSE,
                          overwrite = TRUE
        )

        ## perform the check after the upload for the while loop
        DBrows <- DBI::dbGetQuery(odbcConnect,checkSQL)[1,1]

      }

      ## success!
      message(paste0(nrow(x),
                     ' records written to [',
                     database,'].[dbo].[',tablename,'] in ',
                     round(difftime(Sys.time(),
                                    timeStart,
                                    units = 'mins')),
                     'min')
      )
    }else{
      message('data empty')
    }
  }else{
    message('data does not exists')
  }

  # close the connection
  DBI::dbDisconnect(odbcConnect)
}
