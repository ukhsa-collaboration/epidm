# Grouping of intervals or events in time together

**\[stable\]**

Group across multiple observations of overlapping time intervals, with
defined start and end dates, or events within a static/fixed or rolling
window of time. These are commonly used with inpatient HES/SUS data to
group spells with defined start and end dates, or to group positive
specimen tests, based on specimen dates together into infection
episodes.

## Usage

``` r
group_time(
  x,
  date_start,
  date_end,
  window,
  window_type = c("rolling", "static"),
  group_vars,
  indx_varname = "indx",
  min_varname = "date_min",
  max_varname = "date_max",
  .forceCopy = FALSE
)
```

## Arguments

- x:

  data frame, this will be converted to a data.table

- date_start:

  column containing the start dates for the grouping, provided quoted

- date_end:

  column containing the end dates for the *interval*, quoted

- window:

  an integer representing a time window in days which will be applied to
  the start date for grouping *events*

- window_type:

  character, to determine if a 'rolling' or 'static' grouping method
  should be used when grouping *events*

- group_vars:

  in a vector, the all columns used to group records, quoted

- indx_varname:

  a character string to set variable name for the index column which
  provides a grouping key; default is indx

- min_varname:

  a character string to set variable name for the time period minimum

- max_varname:

  a character string set variable name for the time period maximum

- .forceCopy:

  default FALSE; TRUE will force data.table to take a copy instead of
  editing the data without reference

## Value

the original data.frame as a data.table with the following new fields:

- `indx`; renamed using `indx_varname`:

  an id field for the new aggregated events/intervals; note that where
  the `date_start` is NA, an `indx` value will also be NA

- `min_date`; renamed using `min_varname`:

  the start date for the aggregated events/intervals

- `max_date`; renamed using `max_varname`:

  the end date for the aggregated events/intervals

## Examples

``` r
episode_test <- structure(
  list(
    pat_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L,
               1L, 1L, 1L, 1L, 2L, 2L, 2L),
    species = c(rep("E. coli",7),rep("K. pneumonia",7)),
    spec_type = c(rep("Blood",7),rep("Blood",4),rep("Sputum",3)),
    sp_date = structure(c(18262, 18263, 18281, 18282, 18262, 18263, 18281,
                          18265, 18270, 18281, 18283, 18259, 18260, 18281),
                        class = "Date")
  ),
  row.names = c(NA, -14L), class = "data.frame")

group_time(x=episode_test,
           date_start='sp_date',
           window=14,
           window_type = 'static',
           indx_varname = 'static_indx',
           group_vars=c('pat_id','species','spec_type'))[]
#>     pat_id      species spec_type    sp_date static_indx
#>      <int>       <char>    <char>     <Date>      <char>
#>  1:      1      E. coli     Blood 2020-01-01       1.4.1
#>  2:      1      E. coli     Blood 2020-01-02       1.4.1
#>  3:      1      E. coli     Blood 2020-01-20       1.4.2
#>  4:      1      E. coli     Blood 2020-01-21       1.4.2
#>  5:      1 K. pneumonia     Blood 2020-01-04       2.4.1
#>  6:      1 K. pneumonia     Blood 2020-01-09       2.4.1
#>  7:      1 K. pneumonia     Blood 2020-01-20       2.4.2
#>  8:      1 K. pneumonia     Blood 2020-01-22       2.4.2
#>  9:      2      E. coli     Blood 2020-01-01       3.3.1
#> 10:      2      E. coli     Blood 2020-01-02       3.3.1
#> 11:      2      E. coli     Blood 2020-01-20       3.3.2
#> 12:      2 K. pneumonia    Sputum 2019-12-29       4.3.1
#> 13:      2 K. pneumonia    Sputum 2019-12-30       4.3.1
#> 14:      2 K. pneumonia    Sputum 2020-01-20       4.3.2

spell_test <- data.frame(
  id = c(rep(99,6),rep(88,4),rep(3,3)),
  provider = c("YXZ",rep("ZXY",5),rep("XYZ",4),rep("YZX",3)),
  spell_start = as.Date(
    c(
      "2020-03-01",
      "2020-07-07",
      "2020-02-08",
      "2020-04-28",
      "2020-03-15",
      "2020-07-01",
      "2020-01-01",
      "2020-01-12",
      "2019-12-25",
      "2020-03-28",
      "2020-01-01",
      rep(NA,2)
    )
  ),
  spell_end = as.Date(
    c(
      "2020-03-10",
      "2020-07-26",
      "2020-05-22",
      "2020-04-30",
      "2020-05-20",
      "2020-07-08",
      "2020-01-23",
      "2020-03-30",
      "2020-01-02",
      "2020-04-20",
      "2020-01-01",
      rep(NA,2)
    )
  )
)

group_time(x = spell_test,
           date_start = 'spell_start',
           date_end = 'spell_end',
           group_vars = c('id','provider'),
           indx_varname = 'spell_id',
           min_varname = 'spell_min_date',
           max_varname = 'spell_max_date')[]
#> Error in group_time(x = spell_test, date_start = "spell_start", date_end = "spell_end",     group_vars = c("id", "provider"), indx_varname = "spell_id",     min_varname = "spell_min_date", max_varname = "spell_max_date"): argument "window" is missing, with no default
```
