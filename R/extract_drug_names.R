#' Extract drug names
#'
#' Extracts named drug names from an "intervention" character string,
#' such as in the column provided by the `extract_interventions()`
#' function. This function attempts to strip out dosing, route, and
#' other descriptors, and returns a character string in case of a
#' single drug name found, or a list of character strings in case
#' there are many drug names found.
#'
#' @param intervention A character string containing an unprocessed
#'   intervention name (e.g. "Aspirin 150 mg" will return "Aspirin")
#'   or NA in the case that there is none
#'
#' @return A character string or list of strings containing the drug
#'   name(s) found by the matching algorithm, if any
#'
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#'
#' extract_drug_names("Aspirin 150 mg")
#'
#' extract_drug_names("Paclitaxel 45 mg/m^2")
#'
#' extract_drug_names("Questionnaire administration")
#'
#' extract_drug_names("Combination of two marketed drugs (irinotecan and cisplatin)")

extract_drug_names <- function(intervention) {

  out <- tryCatch({

    ## The pattern that any drug name has to match (includes letters,
    ## numbers, hyphens, periods and spaces)
    drug_regex <- "\\b[A-Za-z0-9-\\.]+(?:\\s[A-Za-z0-9-\\.]+)*\\b"
    
    ## Remove dose information and percents
    intervention <- stringr::str_replace_all(
      intervention,
      stringr::regex(
        "\\b\\d+(\\.?\\d*)?\\s?(mg/kg|mg/ml|mg/tid|mg/m2|mg|ml)\\b",
        ignore_case = TRUE
      ), "") %>%
      stringr::str_replace_all("^\\d+(\\.?\\d*)?%\\s", "") %>% ## "0.9% sodium chloride"
      stringr::str_replace_all("\\s\\d+(\\.?\\d*)?%\\s", "") %>% ## "drug 10% A"
      stringr::str_replace_all("\\(\\d+(\\.?\\d*)?%\\s", "(") %>% ## "drug (10% A)"
      stringr::str_replace_all("\\s\\d+(\\.?\\d*)?%\\)", ")") %>% ## "drug (A 10%)"
      stringr::str_replace_all("(\\s|\\(|\\))\\d+(\\.?\\d*)?%(\\s|\\(|\\))", "") ## "drug (10%%)"

    ## Phrases to remove before pulling out matches
    remove_before_matching <- c(
      "\\bhigh dose\\b",
      "\\blow dose\\b",
      "\\bsingle dose\\b",
      "\\bsingle ascending dose(|s)\\b",
      "\\bstandard dose\\b",
      "\\bintermediate dose\\b",
      "\\bfixed repeated dose\\b",
      "\\bfixed dose\\b",
      "\\brepeated dose\\b",
      "\\bup to\\b",
      "\\bdose level [A-Za-z0-9]+\\b",
      "\\bPharmacological\\b",
      "\\bLaboratory\\b",
      "\\bBiomarker\\b",
      "\\bdiagnostic\\b",
      "\\bProtocol\\b",
      "\\bdrug\\b",
      "\\bdrugs\\b",
      "\\bgel\\b",
      "\\btopical\\b",
      "\\bpressurized\\b",
      "\\btablet\\b",
      "\\btablets\\b",
      "\\bliposome injection\\b",
      "\\binjectable\\b",
      "\\bInjection\\b",
      "\\bInjections\\b",
      "\\bInj\\b",
      "\\bMedical System\\b",
      "\\bSystem\\b",
      "\\bPhoton Beam\\b",
      "\\bplatinum agent\\b",
      "\\bContinuation of\\b",
      "\\bDiscontinuation of\\b",
      "\\bAgent group\\b",
      "\\bAgent\\b",
      "\\bAntiplatelet\\b",
      "\\bChemotherapy\\b",
      "\\bTherapy\\b",
      "\\bStudy\\b",
      "\\bAssessment\\b",
      "\\bAnalysis\\b",
      "\\bHormonal\\b",
      "\\bstem cell(|s)\\b",
      "\\bautologous\\b",
      "\\ballogeneic\\b",
      "\\btransplantation\\b",
      "\\bhypomethylating\\b",
      "\\bnatural killer\\b",
      "\\b[A-Za-z0-9-\\s]+\\st(\\s|-)cell(|s)\\b",
      "\\bcell\\b",
      "\\bsurgery\\b",
      "\\bfor tumor removal\\b",
      "\\bresection\\b",
      "\\bPathway\\b",
      "\\bSignalling\\b",
      "\\bNotch\\b",
      "\\bInactive\\b",
      "\\bintravenous solution\\b",
      "\\bsolution\\b",
      "\\bmonotherapy\\b",
      "\\bcombination of\\b",
      "\\bcombination\\b",
      "\\bintensity\\b",
      "\\brecommended phase 2 dose\\b",
      "\\boral capsule\\b",
      "\\boral\\b",
      "\\bcapsule(|s)\\b",
      "\\balone\\b",
      "\\bwithout\\b",
      "\\bformerly\\b",
      "\\bmonoclonal\\b",
      "\\bantibody\\b",
      "\\bmodified\\b",
      "\\bradiation\\b",
      "\\bfractionated\\b",
      "\\binduction\\b",
      "\\bmaintenance\\b",
      "\\bmodulated\\b",
      "\\bherbal\\b",
      "\\bdecoction\\b",
      "\\binvestigator's choice\\b",
      "\\b([A-Za-z0-9 \\.\\(\\)]+)?imaging( device|)\\b",
      "\\badministration\\b",
      "\\bintrapleural\\b",
      "\\bin situ hybridization\\b",
      "\\bchromogenic\\b",
      "\\bfluorescence\\b",
      "\\bgene expression\\b",
      "\\bengineered\\b",
      "\\btransplant\\b",
      "\\bincreasing\\b",
      "\\bsuspension\\b",
      "\\bnanoparticles\\b",
      "\\bfrozen\\b",
      "\\bliquid\\b",
      "\\blyophilized powder\\b",
      "\\bpowder\\b",
      "\\bmatching placebo\\b",
      "\\bplacebo\\b",
      "\\bquality-of-life\\b",
      "\\bquality of life\\b",
      "\\binduction\\b",
      "\\badditional\\b",
      "\\bphase [0-9]\\b",
      "\\bphase [i]+\\b",
      "\\bintervention [A-Za-z0-9]\\b",
      "\\binspiratory\\b",
      "\\bstandard of care\\b",
      "\\bstandard care\\b",
      "\\bTreatment as usual\\b",
      "\\b[A-Za-z0-9 ]+ intervention\\b",
      "\\breminder text\\b",
      "\\bextended consultation\\b",
      "\\bconsultation\\b",
      "\\bfor\\b",
      "\\bxenograft\\b",
      "\\bonly\\b",
      "\\bUS-sourced\\b",
      "\\bEU-sourced\\b",
      "\\bcommercially available\\b",
      "\\bopen sinus lifting\\b",
      "\\bquantitative sensory test [A-Za-z0-9]\\b",
      "\\bvignette\\b",
      "\\bdiary\\b",
      "\\bdairy\\b",
      "\\blog\\b",
      "\\bnormal educational material\\b",
      "\\blow fat\\b",
      "\\bhigh fat\\b",
      "\\bmeal\\b",
      "\\b[A-Za-z0-9 ]+ device\\b",
      "\\bcomparator\\b",
      "\\bdosing regimen [A-Za-z0-9]\\b",
      "\\bmedical examination\\b",
      "\\b\\d* day cycle\\b",
      "\\b[A-Za-z0-9\\s]+ embolization$\\b",
      "\\b[A-Za-z0-9-]+(\\s?|-)targeted checkpoint inhibitor\\b",
      "\\b[A-Za-z0-9-]+(\\s?|-)inhibitor\\b",
      "\\binhibitor\\b",
      "\\b[A-Za-z0-9-]+(\\s?|-)specific\\b",
      "\\banti-[A-Za-z0-9-]+\\b",
      "\\barm\\s*[A-Za-z0-9]+\\b",
      "\\bcohort\\s*[A-Za-z0-9]+\\b",
      "\\bwedge\\s*[A-Za-z0-9]+\\b",
      "^evaluation of [A-Za-z0-9 '\\(\\)-:]+",
      "^guideline(\\s|-)directed\\b([A-Za-z0-9 \\(\\)]+)?",
      "^web(\\s|-)based\\b([A-Za-z0-9 \\(\\)]+)?",
      "^face(\\s|-)to(\\s|-)face\\b([A-Za-z0-9 \\(\\)]+)?"
    )

    for (i in 1:length(remove_before_matching)) {
      intervention <- stringr::str_replace_all(
        intervention,
        stringr::regex(
          remove_before_matching[[i]],
          ignore_case = TRUE
        ),
        ""
      )
    }
    
    intervention <- stringr::str_trim(intervention)

    ## Replace conjunctions with a comma
    intervention <- stringr::str_replace_all(
      intervention,
      stringr::regex(
        "\\b(and|or|plus|combined with|with|in)\\b|[+]",
        ignore_case = TRUE
      ),
      ","
    )

    ## Split string into a list using commas
    matches <- unlist(stringr::str_split(intervention, ",")) %>%
      trimws() %>%
      stringr::str_extract_all(drug_regex) %>%
      unlist()
    
    ## Words to remove after matching; order matters (higher in the
    ## list is removed first)
    remove_after_matching <- c(
      "^Injection$",
      "^Cohort$",
      "^Analysis$",
      "^Therapy$",
      "^Study$",
      "^Assessment$",
      "^Placebo$",
      "^[A-Za-z0-9\\s]+ Questionnaire$",
      "([A-Za-z0-9\\s]+)?exercise(|s)( program)?",
      "[A-Za-z0-9\\s]+ website",
      "^Questionnaire$",
      "^tablets$",
      "^Tablet$",
      "^Laboratory$",
      "^Biomarker$",
      "^Pharmacological Study$",
      "^IMRT$",
      "^Phase [A-Za-z0-9]+$",
      "^Radiation$",
      "^Dendritic$",
      "^DC$",
      "^Complete$",
      "\\bwaiting period\\b",
      "\\busual care\\b",
      "^HMA$",
      "^gamma-secretase$",
      "^PD-1 targeted checkpoint$",
      "^in$",
      "^OncoSec$",
      "^OMS$",
      "^Monotherapy$",
      "^two marketed$",
      "^PIPAC$",
      "^IntraPeritoneal Air-flow$",
      "^cutaneous$",
      "^ocular$",
      "^Electro-Hyperthermia$",
      "^mEHT$",
      "^TCM$",
      "^of$",
      "^EBV-CTLs$",
      "^MRI$",
      "^an$",
      "^ASCT$",
      "^for$",
      "^t cells$",
      "^Biospecimen Collection$",
      "^room air$",
      "^oxygen$",
      "buzzy bee",
      "vapocoolent",
      "^Cricoid pressure$",
      "^Paratracheal pressure$",
      "^Cerebellar Cognitive-Affective Syndrome Scale$",
      "^IV$",
      "^SC$",
      "[A-Za-z0-9\\s-]+ music$",
      "[A-Za-z0-9\\s-]+ recordings$",
      "^tobacco smoking substitution products$",
      "^saline$",
      "([A-Za-z0-9\\s\\(\\)]+)?\\bVaccine",
      "^SD$",
      "^LD$",
      "cigarette",
      "shave margin",
      "([A-Za-z0-9 ]+)?physiotherapy",
      "[A-Za-z0-9 ]+ outreach",
      "[A-Za-z0-9 ]+ testing",
      "[A-Za-z0-9 ]+ decision(|s) support(|s)",
      "[A-Za-z0-9 ]+ counseling",
      "([A-Za-z0-9 ]+)?messaging intervention",
      "[A-Za-z0-9 ]+ promotion",
      "[A-Za-z0-9 ]+ care",
      "[A-Za-z0-9 ]+ situation",
      "[A-Za-z0-9 ]+ scan",
      "[A-Za-z0-9 ]+ mastectomy",
      "[A-Za-z0-9 ]+ planning",
      "SOC",
      "SUHRI",
      "EUC",
      "control",
      "no imagery",
      "([A-Za-z0-9 -]+)?(\\b|-)plasma",
      "prp",
      "AA",
      "([A-Za-z0-9 ]+)?\\bTomography$",
      "([A-Za-z0-9 ]+)?\\baspiration$",
      "([A-Za-z0-9 ]+)?\\bcollection$",
      "([A-Za-z0-9 ]+)?\\bradiotherapy$",
      "([A-Za-z0-9 ]+)?\\bhealth risk intervention$",
      "([A-Za-z0-9 ]+)?\\bTask$",
      "([A-Za-z0-9 ]+)?\\bBath$",
      "([A-Za-z0-9 ]+)?\\bLens$",
      "([A-Za-z0-9 ]+)?\\bcounseling$",
      "([A-Za-z0-9 ]+)?\\bprocedure$",
      "([A-Za-z0-9 ]+)?\\bRemediation$",
      "([A-Za-z0-9 ]+)?\\bStrategy$",
      "([A-Za-z0-9 ]+)?\\bParticles$",
      "\\bMilk$",
      "\\bDrink$",
      "^cow$",
      "\\bInteractive Film\\b",
      "^active sham ([A-Za-z0-9 ]+)?",
      "calling via telephone",
      "^sitting$",
      "^second$",
      "\\bnurse counselling\\b([A-Za-z0-9 ]+)?",
      "^ccm$",
      "^cbt$",
      "^Cognitive Behavioral$",
      "^fmri$"
    )
    
    ## Remove the `exclude terms`
    for (j in 1:length(remove_after_matching)) {
      matches <- matches[! grepl(
        remove_after_matching[[j]],
        matches,
        ignore.case=TRUE
      )]
    }

    ## Remove anything that's a single character
    matches <- matches[nchar(matches) > 1]
    
    ## Return matches if any, otherwise NA
    if (length(matches) > 0) {
      return(matches)
    } else {
      return(NA)
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
