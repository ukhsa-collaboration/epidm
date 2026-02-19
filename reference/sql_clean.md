# Clean and Read a SQL query

**\[stable\]**

A utility function to read in a SQL query from a character object,
clipboard or text file and remove all comments for use with database
query packages

## Usage

``` r
sql_clean(sql)
```

## Arguments

- sql:

  a SQL file or text string

## Value

a cleaned SQL query without comments as a character string

## Examples

``` r
testSQL <- c(
"/********* INTRO HEADER COMMENTS",
"*********/",
"  SELECT ",
"  [VAR 1]  -- with comments",
",[VAR 2]",",[VAR 3]",
"FROM DATASET ","-- output here")
sql_clean(testSQL)
#> [1] "SELECT [VAR 1] ,[VAR 2] ,[VAR 3] FROM DATASET"
```
