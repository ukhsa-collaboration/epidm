# Connect to a SQL database

**\[stable\]**

An function to help setup connections to SQL databases acting as a
wrapper for the odbc and DBI packages. Used by other sql\_\* tools
within epidm. This uses the credential manager within the system and
assumes you are using a trusted connection.

## Usage

``` r
sql_connect(server, database)
```

## Arguments

- server:

  a string containing the server connection; note that servers may
  require the use of double backslash `\\`

- database:

  a string containing the database name within the data store

## Value

a SQL connection object

## See also

sql_clean sql_read sql_write

## Examples

``` r
if (FALSE) { # \dontrun{
sql <- list(
  dsn = list(ser = 'covid.ukhsa.gov.uk',
             dbn = 'infections')
)

sgss_con = sql_connect(server = sql$dsn$ser, database = sql$dsn$dbn)
} # }
```
