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
    drug_regex <- "\\b[A-Za-z0-9-\\.]+(?:,[A-Za-z0-9-\\.]+)*(?:\\s[A-Za-z0-9-\\.]+(?:,[A-Za-z0-9-\\.]+)*)*\\b"
    
    ## Remove dose information, administration timing and percents
    intervention <- stringr::str_replace_all(
      intervention,
      stringr::regex( ## Removes dosing
        "\\b\\d+(\\.?\\d*)?\\s?(mg/kg|mg/ml|ml/kg|mg/tid|mg/m2|mg|ml|g|Gy)\\b",
        ignore_case = TRUE
      ), ""
    ) %>%
      stringr::str_replace_all("^\\d+(\\.?\\d*)?%\\s", "") %>% ## "0.9% sodium chloride"
      stringr::str_replace_all("\\s\\d+(\\.?\\d*)?%(\\s|$)", "") %>% ## "drug 10% A"
      stringr::str_replace_all("\\(\\d+(\\.?\\d*)?%\\s", "(") %>% ## "drug (10% A)"
      stringr::str_replace_all("\\s\\d+(\\.?\\d*)?%\\)", ")") %>% ## "drug (A 10%)"
      stringr::str_replace_all("(\\s|\\(|\\))\\d+(\\.?\\d*)?%(\\s|\\(|\\))", "") ## "drug (10%)"
      
      ## To remove before pulling out matches
      remove_before_matching <- c(
        "\u00AE", ## The (R) sign
        "(?<!\\w)e\\.v\\.(?!\\w)",
        "(?<!\\w)i\\.v\\.(?!\\w)",
        "\\bQ[0-9]+W\\b",
        "\\bQ[0-9]+D\\b",
        "\\bIV\\b",
        "\\bevery\\b",
        "\\b\\d+(\\.?\\d*)?(-|\\s)?week(s)?\\b",
        "\\bq(\\s)?\\d+(\\.?\\d*)?\\b",
        "\\bDay [0-9]+\\b",
        "\\bhigh(-|\\s)dose\\b",
        "\\blow(-|\\s)dose\\b",
        "\\bMedium(-|\\s)dose\\b",
        "\\bLow(-|\\s)Level\\b",
        "\\bHigh(-|\\s)Level\\b",
        "\\bLow intensity\\b",
        "\\bHigh intensity\\b",
        "\\bLong(-)?Term\\b",
        "\\bShort(-)?Term\\b",
        "\\bsingle dose\\b",
        "\\bLow level\\b",
        "\\bHigh level\\b",
        "\\bIntravenous\\b",
        "\\bIntranasal\\b",
        "\\bSublingual\\b",
        "\\bPeripheral\\b",
        "\\bPeroral\\b",
        "\\bsingle ascending dose(|s)\\b",
        "\\bstandard dose\\b",
        "\\bintermediate dose\\b",
        "\\bfixed repeated dose\\b",
        "\\bfixed dose\\b",
        "\\brepeated dose\\b",
        "\\bCurrent Clinical Formulation\\b",
        "\\bPrototype Formulation( [I]+)?\\b",
        "\\bup to\\b",
        "\\bSuch as\\b",
        "\\bWeekly\\b",
        "\\bDose level [A-Za-z0-9]+\\b",
        "\\bReduced Dose [A-Za-z0-9]+\\b",
        "\\bDose [A-Za-z0-9]+\\b",
        "\\bPharmacological\\b",
        "\\bLaboratory\\b",
        "\\bBiomarker\\b",
        "\\bdiagnostic\\b",
        "\\bProtocol\\b",
        "\\bdrug\\b",
        "\\bdrugs\\b",
        "\\bgel\\b",
        "\\btopical\\b",
        "\\bOptional\\b",
        "\\bOptionnal\\b",
        "\\bpressurized\\b",
        "\\bIntercalated\\b",
        "\\bHigh energy\\b",
        "\\bLow energy\\b",
        "\\bDensity pulse\\b",
        "\\bElectromagnetic field\\b",
        "\\btablet\\b",
        "\\btablets\\b",
        "\\bliposome injection\\b",
        "\\binjectable\\b",
        "\\bInjection\\b",
        "\\bInjections\\b",
        "\\bInj\\b",
        "\\bMedical System\\b",
        "\\bContinuous\\b",
        "\\bIntermittent\\b",
        "\\bNebulization of\\b",
        "\\bLicensed\\b",
        "\\bSystem\\b",
        "\\bPhoton Beam\\b",
        "\\bplatinum agent\\b",
        "\\bContinuation of\\b",
        "\\bDiscontinuation of\\b",
        "\\bAgent group\\b",
        "\\bAgent\\b",
        "\\bPrefilled syringe\\b",
        "\\bAntiplatelet\\b",
        "\\bChemotherapy\\b",
        "\\bCurrent diabetes therapy\\b",
        "\\bDiabetes therapy\\b",
        "\\bTherapy\\b",
        "\\bStudy\\b",
        "\\bArm$",
        "\\bAssessment\\b",
        "\\bAnalysis\\b",
        "\\bHormonal\\b",
        "\\bMesenchymal\\b",
        "\\bAdipose derived\\b",
        "\\bstem cell(|s)\\b",
        "\\bautologous\\b",
        "\\ballogeneic\\b",
        "\\bHaplo-mismatched\\b",
        "\\bHematopoietic\\b",
        "\\btransplantation\\b",
        "\\bhypomethylating\\b",
        "\\bnatural killer\\b",
        "\\b[A-Za-z0-9-\\s]+\\st(\\s|-)cell(|s)\\b",
        "\\bcell\\b",
        "\\bOpen surgery\\b",
        "\\bSurgery\\b",
        "\\bFluid\\b",
        "\\bfor tumor removal\\b",
        "\\bresection\\b",
        "\\bPathway\\b",
        "\\bSignalling\\b",
        "\\bNotch\\b",
        "\\bInactive\\b",
        "\\bMinimally invasive\\b",
        "\\bSolution\\b",
        "\\bMonotherapy\\b",
        "\\bMono\\b",
        "\\bgroup of\\b",
        "\\bcombination of\\b",
        "\\bcombination\\b",
        "\\bintensity\\b",
        "\\brecommended phase 2 dose\\b",
        "\\bHard capsule(|s)\\b",
        "\\bOral capsule(|s)\\b",
        "\\bPill(s)?\\b",
        "\\bVehicle\\b",
        "\\bAcute oral\\b",
        "\\bOral cap\\b",
        "\\bOral\\b",
        "\\bCapsule(|s)\\b",
        "\\bSupplementation\\b",
        "\\bSupplement(s)?\\b",
        "\\balone\\b",
        "\\bwithout\\b",
        "\\bMatched to\\b",
        "\\bTo match\\b",
        "\\bFormerly\\b",
        "\\bMonoclonal\\b",
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
        "\\bOphthalmic\\b",
        "\\bExperimental\\b",
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
        "\\bplacebo matching\\b",
        "\\bPlacebo of\\b",
        "\\bmatching placebo\\b",
        "\\bplacebo\\b",
        "\\bquality-of-life\\b",
        "\\bquality of life\\b",
        "\\binduction\\b",
        "\\badditional\\b",
        "\\bphase [0-9]\\b",
        "\\bphase [i]+\\b",
        "\\bIntervention [A-Za-z0-9]\\b",
        "\\bInspiratory\\b",
        "\\bStandard of care\\b",
        "\\bStandard care\\b",
        "\\bStandard medical treatment\\b",
        "\\bStandard treatment\\b",
        "\\bTreatment as usual\\b",
        "\\bTreatment arm\\b",
        "^Regular treatment$",
        "\\b[A-Za-z0-9 ]+ intervention\\b",
        "\\breminder text\\b",
        "\\bextended consultation\\b",
        "\\bconsultation\\b",
        "\\bfor\\b",
        "\\bOpen(\\s|-)label extension\\b",
        "\\bxenograft\\b",
        "\\bonly\\b",
        "\\b(Active )?Sham\\b",
        "\\bUS-sourced\\b",
        "\\bEU-sourced\\b",
        "\\bcommercially available\\b",
        "\\bopen sinus lifting\\b",
        "\\bquantitative sensory test [A-Za-z0-9]\\b",
        "\\bVignette\\b",
        "\\bDiary\\b",
        "\\bDairy\\b",
        "\\bLog\\b",
        "\\blow(\\s|-)fat\\b",
        "\\bhigh(\\s|-)fat\\b",
        "\\bFatty meal\\b",
        "\\bmeal\\b",
        "\\b[A-Za-z0-9 ]+ device\\b",
        "\\bcomparator\\b",
        "\\bdosing regimen [A-Za-z0-9]\\b",
        "\\bRegimen\\b",
        "\\bmedical examination\\b",
        "\\b\\d* day cycle\\b",
        "\\b[A-Za-z0-9\\s]+ embolization$\\b",
        "\\b[A-Za-z0-9-]+(\\s?|-)specific\\b",
        "\\banti-[A-Za-z0-9-]+\\b",
        "\\barm\\s*[A-Za-z0-9]+\\b",
        "\\bcohort\\s*[A-Za-z0-9]+\\b",
        "\\bwedge\\s*[A-Za-z0-9]+\\b",
        "^evaluation of [A-Za-z0-9 '\\(\\)-:]+",
        "^guideline(\\s|-)directed\\b([A-Za-z0-9 \\(\\)]+)?",
        "^web(\\s|-)based\\b([A-Za-z0-9 \\(\\)]+)?",
        "^face(\\s|-)to(\\s|-)face\\b([A-Za-z0-9 \\(\\)]+)?",
        "\\bOromucosal\\b",
        "\\bEsophageal doppler\\b",
        "^Information$",
        "^motivation$",
        "\\bSubject\\'s\\b",
        "\\bCOVID-19 antibody\\b",
        "\\bCOVID-19 mRNA\\b",
        "\\bSARS-CoV-2 mRNA\\b",
        "\\bFlu seasonal\\b",
        "\\bSelected\\b",
        "\\bInvestigational\\b",
        "\\bIntervention\\b",
        "\\bFunctional\\b",
        "\\bBiosimilar\\b",
        "\\b\\d+(:?\\d*)? ratio\\b",
        "\\Best Available\\b",
        "\\bVisit \\d+\\b",
        "\\bTreatment$"
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

      ## Replace conjunctions with a double comma
      intervention <- stringr::str_replace_all(
        intervention,
        stringr::regex(
          "\\b(and|or|combined with|with|in)\\b|[+]",
          ignore_case = TRUE
        ),
        ",,"
      )

      ## The case of "plus" is more complicated
      intervention <- stringr::str_replace_all(
        intervention,
        stringr::regex(
          "(?<!-)Plus\\b(?!$)",
          ignore_case = TRUE
        ),
        ",,"
      )
      
      ## Split string into a list using commas
      matches <- unlist(stringr::str_split(intervention, ",,")) %>%
        trimws() %>%
        stringr::str_extract_all(drug_regex) %>%
        unlist()

      ## Replace the matching text in each item with "" (does not
      ## remove the entire match)
      replace_after_matching <- c(
        "([A-Za-z0-9 -]+|)Inhibitor\\b"
      )

      for (j in 1:length(replace_after_matching)) {
        matches <- stringr::str_replace_all(
          matches,
          stringr::regex(
            replace_after_matching[[j]],
            ignore_case = TRUE
          ),
          ""
        ) %>%
          trimws()
      }
      
      ## Remove the entire item in `matches` if it matches anything
      ## here; order matters (higher in the list is removed first)
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
        "^Questionnaire(s)?$",
        "\\bfocus group(s)?$",
        "\\bTechnique(s)?$",
        "^tablets$",
        "^Tablet$",
        "\\bVehicle of\\b",
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
        "\\bEnhanced\\b",
        "^HMA$",
        "^gamma-secretase$",
        "^PD-1 targeted checkpoint$",
        "^in$",
        "^OncoSec$",
        "^OMS$",
        "\\bInhalation\\b",
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
        "^SC$",
        "[A-Za-z0-9\\s-]+ music$",
        "[A-Za-z0-9\\s-]+ recordings$",
        "^tobacco smoking substitution products$",
        "^saline$",
        "([A-Za-z0-9\\s\\(\\)]+)?\\bVaccine",
        "^SD$",
        "^LD$",
        "^cigarette$",
        "^shave margin$",
        "\\bphysiotherapy$",
        "\\boutreach\\b",
        "\\btesting\\b",
        "\\bdecision(|s) support(|s)\\b",
        "\\bcounseling\\b",
        "\\bmessaging intervention\\b",
        "\\bpromotion\\b",
        "\\bcare\\b",
        "\\bsituation\\b",
        "\\bscan\\b",
        "\\bplanning\\b",
        "^SOC$",
        "^SUHRI$",
        "^EUC$",
        "^control$",
        "^no imagery$",
        "(\\b|-)plasma$",
        "^prp$",
        "^AA$",
        "\\bTomography$",
        "\\baspiration$",
        "\\bcollection$",
        "\\bradiotherapy$",
        "\\bhealth risk intervention$",
        "\\bTask$",
        "\\bBath$",
        "\\bLens$",
        "\\bcounseling$",
        "\\bprocedure$",
        "\\bRemediation$",
        "\\bStrategy$",
        "\\bParticles$",
        "\\bMilk$",
        "\\bDrink$",
        "^cow$",
        "^Statin$",
        "\\bInteractive Film\\b",
        "\\bStimulation\\b",
        "calling via telephone",
        "^sitting$",
        "^second$",
        "\\bCounselling\\b([A-Za-z0-9 ]+)?",
        "^ccm$",
        "^cbt$",
        "^Cognitive Behavioral$",
        "^fmri$",
        "\\bEducation material(s)?$",
        "\\bEducation\\b",
        "^printed$",
        "\\bSupportive-expressive treatment$",
        "\\bPeriodontal treatment$",
        "\\bMassage$",
        "^18F FDG PET$",
        "^CT$",
        "^PET$",
        "^vicryl-plus$",
        "^vicryl$",
        "^monocryl-plus$",
        "^monocryl$",
        "^pds-plus$",
        "^pds$",
        "^triclosan$",
        "\\bSutures$",
        "^not coated$",
        "\\bCells$",
        "^anti-CD19 CAR-T cells$",
        "\\bCatheter$",
        "\\bVirtual support$",
        "^LASIK$",
        "^AI(\\s|-)assisted\\b",
        "^croq$",
        "\\bReplacement product$",
        "\\bHealth worker$",
        "\\bScreening$",
        "^non$",
        "\\bTaping$",
        "\\bBiopsy$",
        "\\bReferral$",
        "^onsite treatment$",
        "^family based treatment$",
        "^ampoule$",
        "^bag$",
        "\\bAllograft$",
        "^transcranial direct current stimulation$",
        "\\bCatheter$",
        "^nutrition$",
        "^smoking cessation$",
        "\\bArt exhibition\\b",
        "\\bNeurofeedback\\b",
        "\\bRevascularization$",
        "\\bPsychiatry$",
        "^group process$",
        "^children$",
        "\\bModel$",
        "\\bPlasmablasts\\b",
        "^Prescription slip$",
        "^eyeglasses$",
        "^best practice$",
        "^patient navigation$",
        "^Sham-feedback$",
        "\\bTraining\b",
        "^LIFE$",
        "^standard of practice$",
        "^identifying$",
        "^the victim$",
        "^the aggressor$",
        "\\bPatient observation\\b",
        "\\baccupuncture\\b",
        "\\bLaser\\b",
        "\\bBreakfast\\b",
        "^Intervention group$",
        "^Treatment group$",
        "^Control group$",
        "\\bRehabilitation\\b",
        "\\bManeuvers\\b",
        "\\bDecision aid$",
        "^conventional method$",
        "^behavioral skills$",
        "\\bDigital platform$",
        "\\bLearning activities$",
        "^psychological support$",
        "^ultrasound facilitated$",
        "\\bFibrinolysis$",
        "^minimalist approach$",
        "\\bTissue matrix$",
        "^no change$",
        "^timing$",
        "^light exposure$",
        "\\bFeeding$",
        "^conditioned pain modulation$",
        "^CPM$",
        "^HMCT$",
        "\\bDigital tool(|s)$",
        "\\bGroup sessions$",
        "^EMI$",
        "^ecological momentary interventions$",
        "\\bEducation session(|s)$",
        "\\bMonitoring$",
        "\\bWear first$",
        "^phototherapy$",
        "\\bApp group$",
        "\\bMobile$",
        "\\btext messaging$",
        "^TM$",
        "\\bCamera$",
        "\\bPeriodontal$",
        "\\bAssessment(s)?$",
        "\\bNerve block$",
        "\\bNurse support$",
        "\\bMobile application$",
        "\\bValve repair$",
        "\\bMobile health\\b",
        "\\bMhealth\\b",
        "\\bPatient(-)?reported outcome(s)?\\b",
        "^PRO$",
        "^tool$",
        "\\bGroceries$",
        "\\bBiopsy$",
        "\\bShopping$",
        "^Baseline\\b",
        "\\bSpeech motor chaining$",
        "\\bProbiotic(s)?$",
        "\\bTherapeutic community$",
        "\\bTraining$",
        "^Intermediate$",
        "\\bInterview\\b",
        "\\bAugmentation$",
        "\\bProgram(me)?$",
        "\\bRehabilitation\\b",
        "^upper$",
        "^multivitamin$",
        "^Corticosteroid$",
        "\\bGame$",
        "^Baseline$",
        "\\bCampaign$",
        "^MYH$",
        "^managing your health$",
        "\\bDiet$",
        "\\bNeural foramina$",
        "^Shot$",
        "\\bImplant$",
        "\\bStimulation$",
        "\\bSchedule$",
        "\\bBaseline$",
        "^at the level of\\b",
        "\\bEpidural$",
        "^L[0-9]-S[0-9]$",
        "\\bRecord review$",
        "\\bManagement$",
        "^Vasopressor$",
        "^Fluids$",
        "\\bBiopsy$",
        "\\bSham\\b",
        "^group$",
        "^TMS$",
        "^VR$",
        "\\bVirtual reality$",
        "^Get active$",
        "^TACE$",
        "\\bChemoembolization$",
        "\\bGroup psychotherapy$",
        "^18F$",
        "\\bPET MRI\\b",
        "\\bCT$",
        "^123I$",
        "\\bWarnings$",
        "^Stereotactic body$",
        "\\bExcision$",
        "^endo-surgi$",
        "\\bEndoscopic\\b",
        "\\bNeurostimulation$",
        "^tAN$",
        "\\bTransition$",
        "^H2H$",
        "\\bIrradiation$",
        "\\bConditioning$",
        "^ALLO$",
        "^\\d+(\\.?\\d*)$",
        "^Recombinant\\b",
        "SingleChain",
        "\\bpressure(-|\\s)support\\b",
        "\\bBlocker(s)?$",
        "^Tumor tissue$",
        "^blood draw$",
        "\\bExoskeleton\\b",
        "\\bProtein equivalent\\b",
        "\\bLow-protein\\b",
        "\\bApple\\b",
        "^cGMP-AAs$",
        "\\bTransfer$",
        "\\bLaser$",
        "^cardiology$",
        "^contrast$",
        "^CT$",
        "\\bTests$",
        "\\bPosition$",
        "^no treatment$",
        "^Mindfulness\\b",
        "\\bSurvey(s)?$",
        "^CD19$",
        "^Embedded Clinic$",
        "\\bUltrasound$",
        "\\bReconstruction$",
        "\\bGraft$",
        "\\bRehabilitation$",
        "\\bElevation$",
        "\\bKnee$",
        "ectomy$",
        "\\bScale$",
        "\\bBlock$",
        "\\bRadiosurgery$",
        "^Intrinsic subtyping\\b",
        "\\bBracing$",
        "\\bDevice treatment$",
        "\\bConservative Treatment$",
        "^Stereotactic\\b",
        "\\bApp$",
        "\\bIsolation$",
        "^Catheters$",
        "^x-ray$",
        "^ultrasound$",
        "\\bEating$",
        "^Culturally(-|\\s)Tailored\\b",
        "\\bCannulation method$",
        "^warm application\\b",
        "^Acceptance\\b",
        "^Commitment\\b",
        "^Light$",
        "\\bAn(a)?esthesia$",
        "\\bAn(a)?esthetic$",
        "\\bSimulation$",
        "\\bDissection$",
        "\\bSample(s)?$",
        "\\bResuscitation$",
        "\\bEchocardiographic\\b",
        "\\bGame$",
        "\\bEnvironment$",
        "\\bAgonist$",
        "\\bIntermittent fasting\\b",
        "^Bright light$",
        "^Glasses$",
        "\\bCardiologist$",
        "\\bAccelerators$",
        "\\bFeedback$",
        "\\bDiscussion$",
        "^Participation\\b",
        "\\bTelephone\\b",
        "\\bHypnosis\\b",
        "\\bExamination(s)?$",
        "\\bListening$",
        "\\bAllograft$",
        "\\bAblation$",
        "^the$",
        "^the management of\\b",
        "\\bBrace$",
        "\\bOf \\d+ Grays on\\b",
        "^Silkworm pupa$",
        "\\bReporting$",
        "\\bDevice$",
        "\\bInhibitor$"
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

      ## Remove cases where one of the matches is an acryonym for the
      ## others
      if (length(matches) > 0) {
        for (k in 1:length(matches)) {
          match_of_interest <- matches[[k]]
          other_matches <- matches[-k]

          other_matches_acronym <- substr(other_matches, 1, 1) %>%
            paste(collapse="")

          if (match_of_interest == other_matches_acronym) {
            matches <- matches[-k]
          }
          
        }        
      }
      
      ## Return matches if any, otherwise NA
      if (length(matches) > 0) {
        matches %>%
          unique() %>%
          return()
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
