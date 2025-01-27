#' Extract drug name
#'
#' @param intervention A character string containing an unprocessed
#'   intervention name (e.g. "Aspirin 150 mg" will return "Aspirin")
#'   or NA in the case that there is none
#'
#' @return A character string containing the drug name, if any
#'
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data

extract_drug_name <- function(intervention) {

  out <- tryCatch({

    ## Failure strings: If the intervention matches the following
    ## strings, return NA
    failure <- c(
      "^investigational agent$",
      "^chemotherapy$",
      "^standard of care$",
      "^physician's choice$",
      "^questionnaire$",
      "^injection$",
      "^treatment$",
      "^imaging$",
      "^(high|low) dose$",
      "^biopsy$"
    )

    ## Loop through the ways to fail, and return NA if appropriate
    for (i in 1:length(failure)) {
      if (grepl(failure[i], intervention, ignore.case=TRUE)) {
        return (NA)
      }
    }
    
    extraction <- c(
      "^drug: ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+) and ([A-Za-z0-9-]+)$"
    )

    for (j in 1:length(extraction)) {
      extracted <- stringr::str_match_all(
        intervention,
        extraction[2]
      )
      
      if (! is.na(extracted)) {
        return(extracted)
      }
    }

  },
  error = function (cond) {
    paste0(
      "The following error was raised while downloading data ",
      "by NCT search: \n",
      cond
    ) %>%
      message()
    return(cond)
  },
  warning = function (cond) {
    paste0(
      "The following warning was raised while downloading ",
      "data by NCT search: \n",
      cond
    ) %>%
      message()
    return(cond)
  },
  finally = {
    ## To execute regardless of success or failure
  })

  return(out)
  
}
