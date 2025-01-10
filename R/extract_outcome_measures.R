#' Extract outcome measures
#'
#' Loops through all the entries in a set of trial data from the
#' ClinicalTrials.gov API, extracts outcome measures indexed by NCT
#' Number, one row per outcome measure (meaning possibly more than one
#' row per trial), and returns a data frame with those data. Please
#' note that this function does not download trials from
#' ClinicalTrials.gov; it parses trial data downloaded using the
#' ctgov_ncts() or ctgov_query() function and expects the ordered list
#' provided to be formatted in this manner.
#' 
#' @param ctgovdata A nested list containing all the data provided by
#'     the ClinicalTrials.gov API
#'
#' @return A data frame with one row per outcome measure per trial and
#'   5 columns: `nctid`, the NCT Number for the trial in question,
#'   `outcome_rank` (primary or secondary), `measure` (the name of the
#'   outcome), `description` (a longer description of the outcome),
#'   and `timeframe` (when the outcome is measured).
#' 
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 

extract_outcome_measures <- function (ctgovdata) {

  ## Make an empty data frame to contain trial data
  outcome_measures <- tibble::tribble(
    ~nctid,
    ~outcome_rank,
    ~measure,
    ~description,
    ~timeFrame
  )

  for (i in 1:length(ctgovdata)) {
    
    nctid <- ctgovdata[[i]]$protocolSection$identificationModule$nctId

    ## Get the primary outcomes
    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$outcomesModule$primaryOutcome)) {
      primary <- ctgovdata[[i]]$protocolSection$outcomesModule$primaryOutcome %>%
        purrr::map(tibble::as_tibble) %>%
        purrr::reduce(dplyr::full_join) %>%
        dplyr::mutate("nctid" = nctid) %>%
        dplyr::mutate(outcome_rank = "primary")
    } else {
      primary <- tibble::tribble(
        ~measure,
        ~description,
        ~timeFrame
      )
    }

    ## Get the secondary outcomes
    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$outcomesModule$secondaryOutcomes)) {
      secondary <- ctgovdata[[i]]$protocolSection$outcomesModule$secondaryOutcomes %>%
        purrr::map(tibble::as_tibble) %>%
        purrr::reduce(dplyr::full_join) %>%
        dplyr::mutate("nctid" = nctid) %>%
        dplyr::mutate(outcome_rank = "secondary")      
    } else {
      secondary <- tibble::tribble(
        ~measure,
        ~description,
        ~timeFrame
      )
    }

    outcome_measures <- outcome_measures %>%
      dplyr::bind_rows(primary) %>%
      dplyr::bind_rows(secondary)
    
  }

  return(outcome_measures)
  
}
