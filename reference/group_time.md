# Grouping of intervals or events that occur close together in time

**\[stable\]**

A utility function to group together observations that represent
**overlapping date intervals** (e.g., hospital admission spells) or
**events occurring within a defined time window** e.g., specimen dates
grouped into infection episodes. The function supports both:

• **Interval-based grouping**: records have a start and an end date; any
overlapping intervals are grouped together. • **Event-based grouping**:
records have only a start date and are grouped using either a *static*
or *rolling* time window.

The output provides a unique index per group and the minimum/maximum
date that define the resulting aggregated episode or interval.

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

  A data.frame or data.table containing date variables for grouping.
  Will be converted to a data.table internally.

- date_start:

  Quoted column name giving the start date for each record.

- date_end:

  column containing the end dates for the *interval*, quoted

- window:

  an integer representing a time window in days which will be applied to
  the start date for grouping *events*

- window_type:

  character, to determine if a 'rolling' or 'static' grouping method
  should be used when grouping *events*. A *'static'* window will
  identify the first event, and all records X days from that event will
  be attributed to the same episode. Eg. in a 14 day window, if first
  event is on 01 Mar, and events on day 7 Mar and 14 Mar will be
  grouped, but an event starting 15 Mar days after will start a new
  episode. A *'rolling'* window resets the day counter with each new
  event. Eg. Events on 01 Mar, 07 Mar, 14 Mar and 15 Mar are all
  included in a single episode, as will any additional events up until
  the 29 Mar (assuming a 14-day window).

- group_vars:

  Character vector of quoted column names used to partition the data
  before grouping.

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

  `indx`; renamed using `indx_varname`

  :   an id field for the new aggregated events/intervals; note that
      where the `date_start` is NA, an `indx` value will also be NA

  `min_date`; renamed using `min_varname`

  :   the start date for the aggregated events/intervals

  `max_date`; renamed using `max_varname`

  :   the end date for the aggregated events/intervals

## Value

A `data.table` containing all original columns plus:

## Details

### How the function works

The behaviour depends on whether `date_end` is supplied:

#### 1. **Interval-based grouping (start + end dates)**

If both `date_start` and `date_end` are provided, the function
identifies overlapping intervals within the same `group_vars` grouping.
Any intervals that overlap are combined into a single episode.

This method is typically used for:

- Hospital spells (HES/SUS)

- Contact periods or inpatient stays

#### 2. **Event-based grouping (single-date events)**

If only `date_start` is supplied, records are grouped using a **time
window** defined by the `window` argument.

Two approaches are supported:

- **`window_type = "static"`** A fixed window is applied starting from
  the first event in the group. All events occurring within the window
  are grouped until a gap exceeds the threshold, at which point a new
  episode begins.

- **`window_type = "rolling"`** A dynamic window where each event
  extends the episode end point. An event is grouped as long as it
  occurs within `window` days of the most recent event in the same
  episode.

### Handling of missing values

Records missing `date_start` cannot be grouped and are returned with
`indx` = `NA`. These rows are appended back to the final output.

## Workflow context

how `group_time()` might be used in a pipeline **1) SGSS specimen data –
infection episode grouping (event-based)** After organism/specimen
harmonisation (e.g., via [`lookup_recode()`](lookup_recode.md)),
`group_time()` groups specimen dates into infection episodes using a
defined time window. This helps identify clusters of related positive
tests for the same patient and organism.

**2) HES/SUS inpatient data – continuous spell grouping
(interval-based)** When start and end dates of inpatient stays are
available, `group_time()` collapses overlapping intervals into a single
continuous hospital spell. This is used before linking SGSS infection
episodes to inpatient activity.

**3) Integration across datasets** The outputs from `group_time()` are
used downstream to determine whether infection events fall within or
around periods of hospital care, enabling combined SGSS–HES/SUS–ECDS
analyses.

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
#>        id provider spell_start  spell_end spell_id spell_min_date
#>     <num>   <char>      <Date>     <Date>   <char>         <Date>
#>  1:    88      XYZ  2019-12-25 2020-01-02    1.4.0     2019-12-25
#>  2:    88      XYZ  2020-01-01 2020-01-23    1.4.0     2019-12-25
#>  3:     3      YZX  2020-01-01 2020-01-01    2.1.0     2020-01-01
#>  4:    88      XYZ  2020-01-12 2020-03-30    1.4.0     2019-12-25
#>  5:    99      ZXY  2020-02-08 2020-05-22    3.5.0     2020-02-08
#>  6:    99      YXZ  2020-03-01 2020-03-10    4.1.0     2020-03-01
#>  7:    99      ZXY  2020-03-15 2020-05-20    3.5.0     2020-02-08
#>  8:    88      XYZ  2020-03-28 2020-04-20    1.4.0     2019-12-25
#>  9:    99      ZXY  2020-04-28 2020-04-30    3.5.0     2020-02-08
#> 10:    99      ZXY  2020-07-01 2020-07-08    3.5.1     2020-07-01
#> 11:    99      ZXY  2020-07-07 2020-07-26    3.5.1     2020-07-01
#> 12:     3      YZX        <NA>       <NA>     <NA>           <NA>
#> 13:     3      YZX        <NA>       <NA>     <NA>           <NA>
#>     spell_max_date
#>             <Date>
#>  1:     2020-04-20
#>  2:     2020-04-20
#>  3:     2020-01-01
#>  4:     2020-04-20
#>  5:     2020-05-22
#>  6:     2020-03-10
#>  7:     2020-05-22
#>  8:     2020-04-20
#>  9:     2020-05-22
#> 10:     2020-07-26
#> 11:     2020-07-26
#> 12:           <NA>
#> 13:           <NA>
```
