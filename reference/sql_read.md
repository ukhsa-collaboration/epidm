# Read a table from a SQL database

**\[stable\]**

Read a table object to a SQL database. Acts a wrapper for odbc and DBI
packages.

## Usage

``` r
sql_read(server, database, sql)
```

## Arguments

- server:

  a string containing the server connection

- database:

  a string containing the database name within the data store

- sql:

  a string containing a SQL query or to a .sql/.txt SQL query

## Value

a table from a SQL database

## See also

sql_clean sql_connect
