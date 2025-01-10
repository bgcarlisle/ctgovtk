# ctgovtk

A toolkit for interacting with ClinicalTrials.gov

## Installation

Currently not on CRAN, so you have to install it via `devtools`:

```
install.packages("devtools")
library(devtools)
install_github("bgcarlisle/ctgovtk")
library(ctgovtk)

```

## ClinicalTrials.gov API

This package is a wrapper and tools for the ClinicalTrials.gov API, which is documented at <https://clinicaltrials.gov/data-api/api>.

## Functions provided by `ctgovtk`

### `ctgov_ncts()`

Download a set of trial data from the ClinicalTrials.gov API by their
NCT Numbers.

Returns a nested list containing all the data provided by the
ClinicalTrials.gov API for the NCT Numbers provided.

Example:

```
## Load package
library(ctgovtk)

## Download data for two NCT's (you can also pass it a column of NCT
## Numbers from a data frame or a single one as a string)
result <- ctgov_ncts(c("NCT02500121", "NCT06112340"))

## Extract the Brief Title field
result[[2]]$protocolSection$identificationModule$briefTitle

## Extract all the Brief Title fields
for (i in 1:length(result)) {
  ## Print them all out in the R console
  message(
    result[[i]]$protocolSection$identificationModule$briefTitle
  )
}

```

### `ctgov_query()`

Download a set of trial data from the ClinicalTrials.gov API by a
search query. This function posts the query to the API as the content
of `query.term` variable. The API returns a maximum of 1000 trials per
"page" of results, so this function will concatenate the paged results
into a single result variable for you.

Returns a nested list containing all the data provided by the
ClinicalTrials.gov API for the trials matching the search query
provided.

Example:

```
## Load package
library(ctgovtk)

## Download all the trials that were last updated between 2023-01-15
## and 2023-01-20
result <- ctgov_query(
  "AREA[LastUpdatePostDate]RANGE[2023-01-15,2023-01-20]"
)
```

### `extract_basic_info()`

This function takes an ordered list of the type produced by the
functions `ctgov_ncts()` or `ctgov_query()` and extracts the following
trial data for each one, indexed by their NCT Number, and returns a
data frame with one row per trial contained in the provided ordered
list and three columns:

* `nctid`, the NCT Number of the trial in question
* `brief_title`
* `official_title`
* `overall_status`
* `phase`
* `enrol`
* `enrol_type`
* `min_age`, the minimum trial participant age of the trial in
  question (or NA if not provided)
* `max_age`, the maximum trial participant age of the trial in
  question (or NA if not provided)
* `sex`
* `healthy_volunteers`
* `study_type`, INTERVENTIONAL or OBSERVATIONAL
* `allocation`
* `intervention_model`
* `masking`
* `primary_purpose`
* `start_date`
* `start_date_type`
* `pc_date`, primary completion date
* `pc_date_type`, primary completion date ACTUAL or ESTIMATED
* `fp_date`, first posted date
* `fp_date_type`, first posted date ACTUAL or ESTIMATED

Example:

```
## Load package
library(ctgovtk)

## Download all the interventional trials with an overall status of
## Completed or Terminated in phases 1-4, launched in 2023 or later
## with a location in USA

query <- paste(
  "SEARCH[Location](AREA[LocationCountry]United States)",
  "AREA[OverallStatus]COVER[FullMatch]('COMPLETED' OR 'TERMINATED')",
  "AREA[Phase]COVER[FullMatch]('PHASE1' OR 'PHASE2' OR 'PHASE3' OR 'PHASE4')",
  "AREA[StudyType]COVER[FullMatch]('INTERVENTIONAL')",
  "AREA[StartDate]RANGE[01/01/2023,MAX]",
  sep = " AND "
)

result <- ctgov_query(query)

## Pull out the minimum and maximum ages for trial participants in the
## clinical trial data returned by this search

trial_data <- extract_basic_info(result)

```

### `extract_outcome_measures()`

This function takes an ordered list of the type produced by the
functions `ctgov_ncts()` or `ctgov_query()` and extracts primary and
secondary outcome measures for each one, indexed by their NCT Number,
and returns a data frame with one row per outcome measure per trial
and 5 columns: `nctid`, the NCT Number for the trial in question,
`outcome_rank` (primary or secondary), `measure` (the name of the
outcome), `description` (a longer description of the outcome), and
`timeframe` (when the outcome is measured).

Example:

```
## Load package
library(ctgovtk)
library(tidyverse)

## Download all trial data for NCT05105412 and NCT06112340 and pull
## out the outcome measures into a table

ctgov_ncts(c("NCT06112340", "NCT01714739")) %>%
  extract_outcome_measures()
```

## How to cite `ctgovtk`

Here is a BibTeX entry for `ctgovtk`:

```
@Manual{bgcarlisle-ctgovtk,
  Title          = {A toolkit for interacting with ClinicalTrials.gov},
  Author         = {Carlisle, Benjamin Gregory},
  Organization   = {The Grey Literature},
  Address        = {Montreal, Canada},
  url            = {https://github.com/bgcarlisle/ctgovtk},
  year           = 2025
}
```

If you use my software in your research and you found it useful, I
would take it as a kindness if you cited it.

Best,

Benjamin Gregory Carlisle PhD
