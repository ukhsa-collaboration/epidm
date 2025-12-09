# Write a table to a SQL database

**\[stable\]**

Write a table object to a SQL database. Acts a wrapper for odbc and DBI
packages with additional checks to ensure upload completes.

## Usage

``` r
sql_write(x, server, database, tablename)
```

## Arguments

- x:

  a data.frame/data.table/tibble object

- server:

  a string containing the server connection

- database:

  a string containing the database name within the data store

- tablename:

  a string containing the chosen SQL database table name

## Value

writes a data.frame/data.table/tibble to a SQL database
