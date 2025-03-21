% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_drug_names.R
\name{extract_drug_names}
\alias{extract_drug_names}
\title{Extract drug names}
\usage{
extract_drug_names(intervention)
}
\arguments{
\item{intervention}{A character string containing an unprocessed
intervention name (e.g. "Aspirin 150 mg" will return "Aspirin")
or NA in the case that there is none}
}
\value{
A character string or list of strings containing the drug
name(s) found by the matching algorithm, if any
}
\description{
Extracts named drug names from an "intervention" character string,
such as in the column provided by the \code{extract_interventions()}
function. This function attempts to strip out dosing, route, and
other descriptors, and returns a character string in case of a
single drug name found, or a list of character strings in case
there are many drug names found.
}
\examples{

extract_drug_names("Aspirin 150 mg")

extract_drug_names("Paclitaxel 45 mg/m^2")

extract_drug_names("Questionnaire administration")

extract_drug_names("Combination of two marketed drugs (irinotecan and cisplatin)")
}
