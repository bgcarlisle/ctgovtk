#' Download a set of trial data from the ClinicalTrials.gov API by
#' their NCT Numbers
#'
#' @param nctids A list or a column of well-formed ClinicalTrials.gov
#'     NCT Numbers, e.g. "NCT00942747". (A capitalized "NCT" followed
#'     by eight numerals with no spaces or hyphens.)
#'
#' @param batch_size The API will not function properly if you try to
#'     pass a query that is too long, so this function breaks the NCT
#'     Numbers provided into "batches" of 500 by default. The number
#'     of NCT Numbers per query can be modified by changing this
#'     variable.
#'
#' @return A nested list containing all the data provided by the
#'     ClinicalTrials.gov API for the NCT Numbers provided.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'

ctgov_ncts <- function (nctids, batch_size = 500) {

  out <- tryCatch({
    
    ## Check that all NCTs are well-formed
    if (sum(grepl("^NCT\\d{8}$", nctids)) != length(nctids)) {
      stop("Input contains NCTs that are not well-formed")
    }

    ## Check that batch size is numeric
    if (! is.numeric(batch_size)) {
      stop(paste0("'", batch_size, "' is not a number"))
    }
    
    ## Check that the batch size is a whole number
    if (batch_size %% 1 != 0) {
      stop(paste0("'", batch_size, "' is not a whole number"))
    }
    
    ## Check that the site is reachable
    if (httr::http_error("https://clinicaltrials.gov")) {
      message("Unable to connect to clinicaltrials.gov")
      return (FALSE)
    }

    trials <- nctids %>%
      tibble::as_tibble() %>%
      dplyr::rename(nctid = "value") %>%
      dplyr::mutate(
        batch = ceiling(
          dplyr::row_number()/batch_size
        )
      )

    ## Empty list to store downloaded JSON data
    json <- c()

    for (i in 1:max(trials$batch)) {
      
      ## Pull out NCT numbers, turn them into a single comma-separated
      ## string
      query_nctids <- trials %>%
        dplyr::filter(.data$batch == i) %>%
        dplyr::pull(.data$nctid) %>%
        paste(collapse=",")

      ## Download the clinical trial registry data for all the
      ## NCTIDs in a single query to a temporary file
      tmp <- tempfile()
      utils::download.file(
        utils::URLencode(
          paste0(
            ## API connexion details:
            "https://clinicaltrials.gov/api/v2/studies?",
            "filter.ids=",
            ## NCT numbers:
            query_nctids
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

    }
    
    return(json)
    
  },
  error = function (cond) {
    paste0(
      "The following error was raised while downloading data ",
      "by NCT search: \n",
      cond
    ) %>%
      message()

    return("Error")
    
  },
  warning = function (cond) {
    paste0(
      "The following warning was raised while downloading ",
      "data by NCT search: \n",
      cond
    ) %>%
      message()

    return("Warning")
    
  },
  finally = {
    ## To execute regardless of success or failure
  })

  return(out)
  
}
