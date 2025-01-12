#' Extract interventions
#'
#' @param ctgovdata A nested list containing all the data provided by
#'     the ClinicalTrials.gov API
#' 
#' @return A data frame with one row per intervention per trial and 6
#'   columns: `nctid`, the NCT Number for the trial in question,
#'   `type`, e.g. DRUG or BIOLOGICAL, `description` a character string
#'   containing a description of the intervention, `armGroupLabels`, a
#'   character string for a label for the group, `otherNames`, a
#'   character string, for other names for the intervention
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'

extract_interventions <- function (ctgovdata) {

  interventions <- tibble::tribble(
    ~nctid,
    ~type,
    ~name,
    ~description,
    ~armGroupLabels,
    ~otherNames
  )

  for (i in 1:length(ctgovdata)) {

    intdata <- ctgovdata[[i]]$protocolSection$armsInterventionsModule$interventions
    nctid <- ctgovdata[[i]]$protocolSection$identificationModule$nctId
    
    if (length(intdata) > 0) {

      for (j in 1:length(intdata)) {

        if (! rlang::is_null(intdata[[j]]$type)) {
          type <- intdata[[j]]$type
        } else {
          type <- NA
        }

        if (! rlang::is_null(intdata[[j]]$name)) {
          name <- intdata[[j]]$name
        } else {
          name <- NA
        }

        if (! rlang::is_null(intdata[[j]]$description)) {
          description <- intdata[[j]]$description
        } else {
          description <- NA
        }
        
        armGroupLabels <- intdata[[j]]$armGroupLabels %>%
          paste(collapse="|")
        if (armGroupLabels == "") {
          armGroupLabels <- NA
        }
        
        otherNames <- intdata[[j]]$otherNames %>%
          paste(collapse="|")
        if (otherNames == "") {
          otherNames <- NA
        }
        
        newrows <- tibble::tribble(
          ~nctid,
          ~type,
          ~name,
          ~description,
          ~armGroupLabels,
          ~otherNames,

          nctid,
          type,
          name,
          description,
          armGroupLabels,
          otherNames
        )
        
        interventions <- interventions %>%
          dplyr::bind_rows(
            newrows
          )
      }
      
    }
    
  }

  return(interventions)
  
}
