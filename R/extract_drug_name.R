#' Extract drug name
#'
#' @param intervention A character string containing an unprocessed
#'   intervention name (e.g. "Aspirin 150 mg" will return "Aspirin")
#'   or NA in the case that there is none
#'
#' @return A character string or list containing the drug name(s)
#'   found by the matching algorithm, if any
#'
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data

extract_drug_name <- function(intervention) {

  out <- tryCatch({

    extraction <- c(
      "^drug: ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+) and ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+) or ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+) plus ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+) plus modified ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+) + ([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+)+([A-Za-z0-9-]+)$",
      "^([A-Za-z0-9-]+)(?:, ([A-Za-z0-9-]+))*$",
      "^([A-Za-z0-9-]+)$"
    )

    extraction <- tibble::tribble(
      ~match,
      ~extract,

      ## Matches "sunitinib malate"
      "^([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+)$",

      ## Matches "Drug: sunitinib malate"
      "^drug: ([A-Za-z0-9 -]+)$",
      "^drug: ([A-Za-z0-9 -]+)$",

      ## Matches "sunitinib malate (drug)"
      "^([A-Za-z0-9 -]+) \\(drug\\)$",
      "^([A-Za-z0-9 -]+) \\(drug\\)$",

      ## Matches "sunitinib malate and sorafenib tosylate"
      "^([A-Za-z0-9 -]+) and ([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+) and ([A-Za-z0-9 -]+)$",
      
      ## Matches "sunitinib malate or sorafenib tosylate"
      "^([A-Za-z0-9 -]+) or ([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+) or ([A-Za-z0-9 -]+)$",

      ## Matches "sunitinib malate plus sorafenib tosylate"
      "^([A-Za-z0-9 -]+) plus ([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+) plus ([A-Za-z0-9 -]+)$",

      ## Matches "sunitinib malate plus modified sorafenib tosylate"
      "^([A-Za-z0-9 -]+) plus modified ([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+) plus modified ([A-Za-z0-9 -]+)$",

      ## Matches "sunitinib malate with sorafenib tosylate"
      "^([A-Za-z0-9 -]+) with ([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+) with ([A-Za-z0-9 -]+)$",

      ## Matches "sunitinib malate + sorafenib tosylate"
      "^([A-Za-z0-9 -]+) \\+ ([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+) \\+ ([A-Za-z0-9 -]+)$",

      ## Matches "sunitinib malate+sorafenib tosylate"
      "^([A-Za-z0-9 -]+)\\+([A-Za-z0-9 -]+)$",
      "^([A-Za-z0-9 -]+)\\+([A-Za-z0-9 -]+)$",

      ## Matches "300 mg sunitinib malate"
      "^[0-9 ]+mg ([A-Za-z0-9 -]+)$",
      "^[0-9 ]+mg ([A-Za-z0-9 -]+)$"
      ## Matches "sunitinib malate 300 mg"
      ## Matches "sunitinib malate 300 mg tablet"
      ## Matches "sunitinib malate, sorafenib tosylate, eribulin
      ## mesylate"
      
    )

    extraction <- extraction %>%
      dplyr::rowwise() %>%
      dplyr::mutate(found = grepl(.data$match, intervention)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(found) %>%
      dplyr::pull(extract)

    ## Pull out matches
    extracted <- stringr::str_match(
      intervention,
      stringr::regex(extraction, ignore_case = TRUE)
    )

    ## Collapse the matrix into a list
    extracted <- c(extracted)

    ## Remove duplicates
    extracted <- unique(extracted)

    ## Remove NA's
    extracted <- extracted[! is.na(extracted)]

    ## Remove the original string
    extracted <- extracted[extracted != intervention]


    ## Failure strings: If the intervention matches the following
    ## strings, return NA
    failure <- c(
      "investigational agent",
      "chemotherapy",
      "standard of care",
      "physician's choice",
      "questionnaire",
      "injection",
      "treatment",
      "imaging",
      "high dose",
      "low dose",
      "biopsy",
      "laboratory biomarker analysis",
      "peripheral stem cell collection",
      "capsules"
    )
    
    ## Remove failure strings
    extracted <- extracted[! extracted %in% failure]
    
    if (length(extracted) > 0) {
      return(extracted)
    } else {
      return (NA)
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
