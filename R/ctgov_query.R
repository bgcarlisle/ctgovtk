#' Download a set of trial data from the ClinicalTrials.gov API by
#' a search query
#'
#' @param query A character string containing a well-formed
#'     ClinicalTrials.gov query in Essie expression syntax
#'
#' @return A nested list containing all the data provided by the
#'     ClinicalTrials.gov API for the trials matching the search query
#'     provided.
#'
#' @export
#'
#' @importFrom magrittr %>%

ctgov_query <- function (query) {
  
  out <- tryCatch({
    
    ## Check that the site is reachable
    if (httr::http_error("https://clinicaltrials.gov")) {
      message("Unable to connect to clinicaltrials.gov")
      return (FALSE)
    }

    ## Empty list to populate with study data
    json <- c()
    ## A variable to contain the next page token
    next_page_token <- ""
    ## A variable to indicate whether or not to continue looping
    ## through download pages
    continue_to_next_page <- TRUE

    i <- 0

    while(continue_to_next_page) {

      message(i)
      i <- i+1

      if (next_page_token != "") {
        page_query <- paste0(
          "pageToken=",
          next_page_token,
          "&"
        )
      } else {
        page_query <- ""
      }
      
      ## Download the clinical trial registry data for all the
      ## NCTIDs in a single query to a temporary file
      tmp <- tempfile()
      utils::download.file(
        utils::URLencode(
          paste0(
            ## API connexion details:
            "https://clinicaltrials.gov/api/v2/studies?",
            "pageSize=1000&",
            page_query,
            "query.term=",
            ## API query
            query
          )
        ),
        tmp,
        quiet = FALSE
      )
      ## Read into memory and combine with previous download
      response <- jsonlite::read_json(tmp)
      json <- c(json, response$studies)
      ## Delete temporary file
      unlink(tmp)

      if (! is.null(response$nextPageToken)) {
        continue_to_next_page <- TRUE
        next_page_token <- response$nextPageToken
      } else {
        continue_to_next_page <- FALSE
        next_page_token <- ""
      }

    }

    return(json)
    
  },
  error = function (cond) {
    paste0(
      "The following error was raised while downloading data ",
      "by API query: \n",
      cond
    ) %>%
      message()

    return("Error")
    
  },
  warning = function (cond) {
    paste0(
      "The following warning was raised while downloading ",
      "data by API query: \n",
      cond
    ) %>%
      message()

    return("Warning")
    
  },
  finally = {
    
  })

  return(out)
}
