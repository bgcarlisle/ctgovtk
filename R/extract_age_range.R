#' Loops through all the entries in a set of trial data from the
#' ClinicalTrials.gov API, extracts the NCT number, minimum age and
#' maximum age for each and returns a data frame with those
#' data. Please note that this function does not download trials from
#' ClinicalTrials.gov; it parses trial data downloaded using the
#' ctgov_ncts() or ctgov_query() function and expects the ordered list
#' provided to be formatted in this manner.
#'
#' @param ctgovdata A nested list containing all the data provided by
#'     the ClinicalTrials.gov API
#'
#' @return A data frame with one row per clinical trial and three
#'     columns: nctid (a character string containing the NCT number
#'     for the trial in question), min_age (a numeric value for the
#'     minimum age for trial participants for the trial in question or
#'     NA if none provided) and max_age (a numeric value for the
#'     maximum age for trial participants for the trial in question or
#'     NA if none provided)
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 

extract_age_range <- function (ctgovdata) {

    ## Make an empty data frame to contain age data
    trial_age <- tibble::tribble(
        ~nctid, ~min_age, ~max_age
        )
    
    ## Loop through all the trials in the provided list and pull out
    ## their minimum and maximum ages
    for (i in 1:length(ctgovdata)) {
        
        nctid <- ctgovdata[[i]]$protocolSection$identificationModule$nctId

        if (! rlang::is_null(ctgovdata[[i]]$protocolSection$eligibilityModule$minimumAge)) {
            min_age <- ctgovdata[[i]]$protocolSection$eligibilityModule$minimumAge %>%
                stringr::str_extract("[0-9]+") %>%
                as.numeric()
        } else {
            min_age <- NA
        }

        if (! rlang::is_null(ctgovdata[[i]]$protocolSection$eligibilityModule$maximumAge)) {
            max_age <- ctgovdata[[i]]$protocolSection$eligibilityModule$maximumAge %>%
                stringr::str_extract("[0-9]+") %>%
                as.numeric()
        } else {
            max_age <- NA
        }

        trial_age <- trial_age %>%
            dplyr::bind_rows(
                tibble::tribble(
                    ~nctid, ~min_age, ~max_age,
                    nctid, min_age, max_age
                )
            )
        
        
    }

    return (trial_age)
    
}
