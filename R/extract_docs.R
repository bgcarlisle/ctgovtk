#' Extract attached study documents
#'
#' Loops through all the entries in a set of trial data from the
#' ClinicalTrials.gov API, extracts study documents indexed by NCT
#' Number, one row per study document (meaning possibly more than one
#' row per trial), and returns a data frame with those data. Please
#' note that this function does not download trials from
#' ClinicalTrials.gov; it parses trial data downloaded using the
#' ctgov_ncts() or ctgov_query() function and expects the ordered list
#' provided to be formatted in this manner.
#'
#' @param ctgovdata A nested list containing all the data provided by
#'     the ClinicalTrials.gov API
#'
#' @return A data frame with one row per study document (there may be
#'   multiple per NCT Number) and 11 columns: `nctid`, the NCT Number
#'   for the trial in question, `typeAbbrev`, the abbreviation for the
#'   type of study document, `hasProtocol`, a Boolean indicating
#'   whether that document contains the study protocol, `hasSap`, a
#'   Boolean indicating whether that document contains the statistical
#'   analysis plan, `hasIcf`, a Boolean indicating whether that
#'   document contains an informed consent form, `label`, a label for
#'   the document, `date`, a date associated with the document,
#'   `uploadDate`, the date that the document was uploaded to
#'   ClinicalTrials.gov, `filename`, the filename associated with the
#'   document in question, `size`, the number of bytes that the file
#'   in question would take up on a hard disk and `url`, the hyperlink
#'   to the download location of the document in question. In the case
#'   that there is no file data for the set of documents provided, the
#'   `url` column will not be generated and only 10 columns will be
#'   returned.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 

extract_docs <- function (ctgovdata) {

  study_docs <- tibble::tribble(
    ~nctid,
    ~typeAbbrev,
    ~hasProtocol,
    ~hasSap,
    ~hasIcf,
    ~label,
    ~date,
    ~uploadDate,
    ~filename,
    ~size
  )

  for (i in 1:length(ctgovdata)) {
    
    nctid <- ctgovdata[[i]]$protocolSection$identificationModule$nctId

    if (! rlang::is_null(ctgovdata[[i]]$documentSection$largeDocumentModule$largeDocs)) {

      study_docs <- study_docs %>%
        dplyr::bind_rows(
          ctgovdata[[i]]$documentSection$largeDocumentModule$largeDocs %>%
            purrr::map(tibble::as_tibble) %>%
            purrr::reduce(dplyr::full_join) %>%
            dplyr::mutate("nctid" = nctid)
        )
      
    }
    
  }

  ## If there is a "file" column
  if ("filename" %in% names(study_docs)) {
    ## Add a new column for the download link
    study_docs <- study_docs %>%
      dplyr::mutate(
        url = paste0(
          "https://cdn.clinicaltrials.gov/large-docs/",
          substr(.data$nctid, 10, 12),
          "/",
          .data$nctid,
          "/",
          .data$filename
        )
      )
  }

  return(study_docs)
  
}
