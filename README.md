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
