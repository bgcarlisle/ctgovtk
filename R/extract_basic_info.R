#' Extract basic trial info
#'
#' Loops through all the entries in a set of trial data from the
#' ClinicalTrials.gov API, extracts basic trial info indexed by NCT
#' Number, one row per trial, and returns a data frame with those
#' data. Please note that this function does not download trials from
#' ClinicalTrials.gov; it parses trial data downloaded using the
#' ctgov_ncts() or ctgov_query() function and expects the ordered list
#' provided to be formatted in this manner.
#'
#' @param ctgovdata A nested list containing all the data provided by
#'     the ClinicalTrials.gov API
#'
#' @return A data frame with one row per clinical trial and three
#'   columns: nctid (a character string containing the NCT number for
#'   the trial in question), brief_title (a character string
#'   containing the brief title for the trial in question),
#'   official_title (a character string containing the official title
#'   for the trial in question), overall_status (a character string
#'   containing the trial's overall status), phase (the phase of the
#'   trial), enrol (enrolment count), enrol_type (ACTUAL or
#'   ESTIMATED), min_age (minimum participant age as an integer or
#'   NA), max_age (maximum participant age as an integer or NA), sex
#'   (character string describing the sex of participants),
#'   healthy_volunteers (Boolean, indicates whether healthy volunteers
#'   are enrolled), study_type (INTERVENTIONAL or OBSERVATIONAL),
#'   start_date (a character string containing an ISO-8601 formatted
#'   date, YYYY-MM-DD or YYYY-MM corresponding to the trial start),
#'   start_date_type (start date ACTUAL or ESTIMATED) pc_date (primary
#'   completion date), pc_date_type (primary completion date ACTUAL or
#'   ESTIMATED), fp_date (first posted date), fp_date_type (first
#'   posted date ACTUAL or ESTIMATED)
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 

extract_basic_info <- function (ctgovdata) {

  ## Make an empty data frame to contain trial data
  trial_data <- tibble::tribble(
    ~nctid,
    ~brief_title,
    ~official_title,
    ~overall_status,
    ~phase,
    ~enrol,
    ~enrol_type,
    ~min_age,
    ~max_age,
    ~sex,
    ~healthy_volunteers,
    ~study_type,
    ~allocation,
    ~intervention_model,
    ~masking,
    ~primary_purpose,
    ~start_date,
    ~start_date_type,
    ~pc_date,
    ~pc_date_type,
    ~fp_date,
    ~fp_date_type
  )

  ## Loop through all the trials in the provided list and pull out
  ## the trial data
  for (i in 1:length(ctgovdata)) {
        
    nctid <- ctgovdata[[i]]$protocolSection$identificationModule$nctId

    if (! rlang::is_null(ctgovdata[[i]]$protocol$identificationModule$briefTitle)) {
      brief_title <- ctgovdata[[i]]$protocol$identificationModule$briefTitle
    } else {
      brief_title <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocol$identificationModule$officialTitle)) {
      official_title <- ctgovdata[[i]]$protocol$identificationModule$officialTitle          
    } else {
      official_title <- NA
    }

    overall_status <- ctgovdata[[i]]$protocol$statusModule$overallStatus

    ## There may be multiple phases associated with a single trial, so
    ## loop through them all and concatenate them with '|' as a
    ## separator
    phases <- c()
    for (j in 1:length(ctgovdata[[i]]$protocolSection$designModule$phases)) {
      phases <- c(phases, ctgovdata[[i]]$protocolSection$designModule$phases[[j]])
    }
    phase <- paste(phases, collapse="|")

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$enrollmentInfo$count)) {
      enrol <- ctgovdata[[i]]$protocolSection$designModule$enrollmentInfo$count
    } else {
        enrol <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$enrollmentInfo$type)) {
      enrol_type <- ctgovdata[[i]]$protocolSection$designModule$enrollmentInfo$type
    } else {
        enrol_type <- NA
    }

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

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$eligibilityModule$sex)) {
      sex <- ctgovdata[[i]]$protocolSection$eligibilityModule$sex
    } else {
      sex <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$eligibilityModule$healthyVolunteers)) {
      healthy_volunteers <- ctgovdata[[i]]$protocolSection$eligibilityModule$healthyVolunteers
    } else {
        healthy_volunteers <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$studyType)) {
      study_type <- ctgovdata[[i]]$protocolSection$designModule$studyType
    } else {
      study_type <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$designInfo$allocation)) {
        allocation <- ctgovdata[[i]]$protocolSection$designModule$designInfo$allocation
    } else {
        allocation <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$designInfo$interventionModel)) {
      intervention_model <- ctgovdata[[i]]$protocolSection$designModule$designInfo$interventionModel
    } else {
      intervention_model <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$designInfo$maskingInfo$masking)) {
      masking <- ctgovdata[[i]]$protocolSection$designModule$designInfo$maskingInfo$masking
    } else {
      masking <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$designModule$designInfo$primaryPurpose)) {
      primary_purpose <- ctgovdata[[i]]$protocolSection$designModule$designInfo$primaryPurpose
    } else {
      primary_purpose <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$statusModule$startDateStruct$date)) {
      start_date <- ctgovdata[[i]]$protocolSection$statusModule$startDateStruct$date
    } else {
        start_date <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$statusModule$startDateStruct$type)) {
      start_date_type <- ctgovdata[[i]]$protocolSection$statusModule$startDateStruct$type
    } else {
      start_date_type <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$statusModule$primaryCompletionDateStruct$date)) {
      pc_date <- ctgovdata[[i]]$protocolSection$statusModule$primaryCompletionDateStruct$date
    } else {
        pc_date <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$statusModule$primaryCompletionDateStruct$type)) {
      pc_date_type <- ctgovdata[[i]]$protocolSection$statusModule$primaryCompletionDateStruct$type
    } else {
      pc_date_type <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$statusModule$studyFirstPostDateStruct$date)) {
      fp_date <- ctgovdata[[i]]$protocolSection$statusModule$studyFirstPostDateStruct$date
    } else {
        fp_date <- NA
    }

    if (! rlang::is_null(ctgovdata[[i]]$protocolSection$statusModule$studyFirstPostDateStruct$type)) {
      fp_date_type <- ctgovdata[[i]]$protocolSection$statusModule$studyFirstPostDateStruct$type
    } else {
        fp_date_type <- NA
    }

    trial_data <- trial_data %>%
      dplyr::bind_rows(
        tibble::tribble(
          ~nctid,
          ~brief_title,
          ~official_title,
          ~overall_status,
          ~phase,
          ~enrol,
          ~enrol_type,
          ~min_age,
          ~max_age,
          ~sex,
          ~healthy_volunteers,
          ~study_type,
          ~allocation,
          ~intervention_model,
          ~masking,
          ~primary_purpose,
          ~start_date,
          ~start_date_type,
          ~pc_date,
          ~pc_date_type,
          ~fp_date,
          ~fp_date_type,
          
          nctid,
          brief_title,
          official_title,
          overall_status,
          phase,
          enrol,
          enrol_type,
          min_age,
          max_age,
          sex,
          healthy_volunteers,
          study_type,
          allocation,
          intervention_model,
          masking,
          primary_purpose,
          start_date,
          start_date_type,
          pc_date,
          pc_date_type,
          fp_date,
          fp_date_type
          
        )
      )
    
  }

  return(trial_data)
   
}
