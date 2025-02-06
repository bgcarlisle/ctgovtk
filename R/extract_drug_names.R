#' Extract drug names
#'
#' Extracts named drug names from the intervention
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

extract_drug_names <- function(intervention) {

  out <- tryCatch({

    ## The pattern that any drug name has to match (includes letters,
    ## numbers, hyphens, periods and spaces)
    drug_regex <- "\\b[A-Za-z0-9-\\.]+(?:\\s[A-Za-z0-9-\\.]+)*\\b"

    remove_before_matching <- c(
      "high dose",
      "low dose",
      "single dose",
      "intermediate dose",
      "fixed repeated dose",
      "fixed dose",
      "repeated dose",
      "Pharmacological",
      "Laboratory",
      "Biomarker",
      "diagnostic",
      "Protocol",
      "drug",
      "drugs",
      "pressurized",
      "tablets",
      "liposome injection",
      "injectable",
      "Injection",
      "Injections",
      "Inj",
      "platinum agent",
      "Agent",
      "Chemotherapy",
      "Therapy",
      "Study",
      "Assessment",
      "Analysis",
      "Hormonal",
      "stem cell(|s)",
      "autologous",
      "allogeneic",
      "transplantation",
      "hypomethylating",
      "akt inhibitor",
      "natural killer",
      "[A-Za-z0-9-\\s]+\\st(\\s|-)cell(|s)",
      "cell",
      "surgery",
      "for tumor removal",
      "resection",
      "hsp90 inhibitor",
      "inhibitor",
      "pathway",
      "signalling",
      "notch",
      "intravenous solution",
      "solution",
      "monotherapy",
      "combination of",
      "combination",
      "radiotherapy",
      "intensity",
      "recommended phase 2 dose",
      "oral capsule",
      "oral",
      "capsule(|s)",
      "alone",
      "without",
      "formerly",
      "monoclonal",
      "antibody",
      "modified",
      "radiation",
      "fractionated",
      "induction",
      "maintenance",
      "modulated",
      "herbal",
      "decoction",
      "investigator's choice",
      "contrast-enhanced magnetic resonance imaging",
      "magnetic resonance imaging",
      "imaging",
      "administration",
      "intrapleural",
      "in situ hybridization",
      "chromogenic",
      "fluorescence",
      "gene expression",
      "engineered",
      "transplant",
      "increasing",
      "suspension",
      "nanoparticles",
      "frozen",
      "liquid",
      "lyophilized powder",
      "powder",
      "matching placebo",
      "placebo",
      "quality-of-life",
      "quality of life",
      "induction",
      "phase [0-9]",
      "phase [i]+"
    )
    
    ## Remove dosage, formulation details, cohort numbers, and
    ## unwanted descriptors prior to matching
    intervention <- stringr::str_replace_all(
      intervention,
      stringr::regex(
        "\\b\\d+(\\.?\\d*)?\\s?(mg/kg|mg/ml|mg/m2|mg|ml|Tablet|Tablets)\\b",
        ignore_case = TRUE
      ), " ") %>%
      stringr::str_replace_all("\\b(c|C)ohort\\s*[A-Za-z0-9]+\\b", "") %>%
      stringr::str_replace_all("\\b(A|a)rm\\s*[A-Za-z0-9]+\\b", "") %>%
      stringr::str_replace_all("\\s[0-9]+%\\s", "") %>%
      stringr::str_replace_all("\\([0-9]+%\\s", "(") %>%
      stringr::str_replace_all("\\s[0-9]+%\\)", ")") %>%
      stringr::str_replace_all("(\\s|\\(|\\))[0-9]+%(\\s|\\(|\\))", "") %>%
      stringr::str_replace_all("\\b\\d* (d|D)ay (c|C)ycle\\b", "") %>%
      stringr::str_replace_all(stringr::regex("\\banti-[A-Za-z0-9-]+\\b", ignore_case = TRUE), "") %>%
      stringr::str_replace_all("\\b[A-Za-z0-9-]+(\\s?|-)(t|T)argeted (c|C)heckpoint (i|I)nhibitor\\b", "") %>%
      stringr::str_replace_all("\\b[A-Za-z0-9-]+(\\s?|-)(t|Targeted) (i|I)nhibitor\\b", "") %>%
      stringr::str_replace_all("\\b[A-Za-z0-9-]+(\\s?|-)(i|I)nhibitor\\b", "") %>%
      stringr::str_replace_all("\\b[A-Za-z0-9-]+(\\s?|-)(s|S)pecific\\b", "")

    for (i in 1:length(remove_before_matching)) {
      intervention <- stringr::str_replace_all(
        intervention,
        stringr::regex(
          paste0(
            "\\b",
            remove_before_matching[[i]],
            "\\b"
          ),
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
    
    ## Words to remove after matching
    exclude_terms <- c(
      "Injection",
      "Cohort",
      "Analysis",
      "Therapy",
      "Study",
      "Assessment",
      "Placebo",
      "Questionnaire",
      "tablets",
      "Tablet",
      "Laboratory",
      "Biomarker",
      "Pharmacological Study",
      "External beam radiotherapy",
      "Intensity-modulated radiotherapy",
      "IMRT",
      "Phase I",
      "Phase 1",
      "Phase Ib",
      "Phase 1b",
      "Phase II",
      "Phase 2",
      "Phase III",
      "Phase 3",
      "Radiation",
      "Dendritic",
      "DC",
      "Complete",
      "HMA",
      "gamma-secretase",
      "PD-1 targeted checkpoint",
      "in",
      "OncoSec Medical System",
      "OMS",
      "Monotherapy",
      "two marketed",
      "PIPAC",
      "IntraPeritoneal Air-flow",
      "cutaneous",
      "ocular",
      "Electro-Hyperthermia",
      "mEHT",
      "TCM",
      "of",
      "EBV-CTLs",
      "MRI",
      "an",
      "ASCT",
      "for",
      "t cells"
    )

    ## Remove the `exclude terms`
    clean_matches <- matches[! tolower(matches) %in% tolower(exclude_terms)]

    ## Remove anything that's a single character
    clean_matches <- clean_matches[nchar(clean_matches) > 1]
    
    ## Return matches if any, otherwise NA
    if (length(clean_matches) > 0) {
      return(clean_matches)
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
