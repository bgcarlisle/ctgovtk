test_that(
  "sunitinib",
  {
    expect_equal(
      extract_drug_names("sunitinib"),
      "sunitinib"
    )
  }
)

test_that(
  "Gemcitabine and Cisplatin",
  {
    expect_equal(
      extract_drug_names("Gemcitabine and Cisplatin"),
      c("Gemcitabine", "Cisplatin")
    )
  }
)

test_that(
  "N,N-Dimethyltryptamine",
  {
    expect_equal(
      extract_drug_names("N,N-Dimethyltryptamine"),
      "N,N-Dimethyltryptamine"
    )
  }
)

test_that(
  "sorafenib, sunitinib",
  {
    expect_equal(
      extract_drug_names("sorafenib, sunitinib"),
      c("sorafenib", "sunitinib")
    )
  }
)

test_that(
  "Donafenib 200mg",
  {
    expect_equal(
      extract_drug_names("Donafenib 200mg"),
      "Donafenib"
    )
  }
)

test_that(
  "Erlotinib + Sorafenib",
  {
    expect_equal(
      extract_drug_names("Erlotinib + Sorafenib"),
      c("Erlotinib", "Sorafenib")
    )
  }
)



test_that(
  "Nab-paclitaxel and S-1",
  {
    expect_equal(
      extract_drug_names("Nab-paclitaxel and S-1"),
      c("Nab-paclitaxel", "S-1")
    )
  }
)

test_that(
  "Laboratory Biomarker Analysis",
  {
    expect_equal(
      extract_drug_names("Laboratory Biomarker Analysis"),
      NA
    )
  }
)

test_that(
  "Nivolumab 40 mg in 4 ml Injection",
  {
    expect_equal(
      extract_drug_names("Nivolumab 40 mg in 4 ml Injection"),
      "Nivolumab"
    )
  }
)

test_that(
  "Atezolizumab Cohort 7",
  {
    expect_equal(
      extract_drug_names("Atezolizumab Cohort 7"),
      "Atezolizumab"
    )
  }
)

test_that(
  "Atezolizumab Cohort 7",
  {
    expect_equal(
      extract_drug_names("Interferon Gamma-1b"),
      "Interferon Gamma-1b"
    )
  }
)

test_that(
  "Nivolumab/Ipilimumab",
  {
    expect_equal(
      extract_drug_names("Nivolumab/Ipilimumab"),
      c("Nivolumab", "Ipilimumab")
    )
  }
)

test_that(
  "aspirin and tylenol",
  {
    expect_equal(
      extract_drug_names("aspirin and tylenol"),
      c("aspirin", "tylenol")
    )
  }
)

test_that(
  "Dexamethasone tablets",
  {
    expect_equal(
      extract_drug_names("Dexamethasone tablets"),
      "Dexamethasone"
    )
  }
)

test_that(
  "Pegylated Interferon alpha-2b",
  {
    expect_equal(
      extract_drug_names("Pegylated Interferon alpha-2b"),
      "Pegylated Interferon alpha-2b"
    )
  }
)

test_that(
  "eribulin mesylate + erlotinib",
  {
    expect_equal(
      extract_drug_names("eribulin mesylate + erlotinib"),
      c("eribulin mesylate", "erlotinib")
    )
  }
)

test_that(
  "Pharmacological Study",
  {
    expect_equal(
      extract_drug_names("Pharmacological Study"),
      NA
    )
  }
)

test_that(
  "Bemcentinib; pembrolizumab",
  {
    expect_equal(
      extract_drug_names("Bemcentinib; pembrolizumab"),
      c("Bemcentinib", "pembrolizumab")
    )
  }
)

test_that(
  "ADI-PEG 20 plus modified FOLFOX6",
  {
    expect_equal(
      extract_drug_names("ADI-PEG 20 plus modified FOLFOX6"),
      c("ADI-PEG 20", "FOLFOX6")
    )
  }
)

test_that(
  "aspirin plus tylenol",
  {
    expect_equal(
      extract_drug_names("aspirin plus tylenol"),
      c("aspirin", "tylenol")
    )
  }
)

test_that(
  "sunitinib plus sorafenib plus dexamethasone",
  {
    expect_equal(
      extract_drug_names("sunitinib plus sorafenib plus dexamethasone"),
      c("sunitinib", "sorafenib", "dexamethasone")
    )
  }
)

test_that(
  "Letrozole 2.5Mg Tablet",
  {
    expect_equal(
      extract_drug_names("Letrozole 2.5Mg Tablet"),
      "Letrozole"
    )
  }
)

test_that(
  "Hormonal Therapy with Anastrozole and Fulvestrant",
  {
    expect_equal(
      extract_drug_names("Hormonal Therapy with Anastrozole and Fulvestrant"),
      c("Anastrozole", "Fulvestrant")
    )
  }
)

test_that(
  "Docetaxel, nedaplatin, fluorouracil",
  {
    expect_equal(
      extract_drug_names("Docetaxel, nedaplatin, fluorouracil"),
      c("Docetaxel", "nedaplatin", "fluorouracil")
    )
  }
)

test_that(
  "autologous or allogeneic stem cell transplantation",
  {
    expect_equal(
      extract_drug_names("autologous or allogeneic stem cell transplantation"),
      NA
    )
  }
)

test_that(
  "Intensity-modulated radiotherapy (IMRT)",
  {
    expect_equal(
      extract_drug_names("Intensity-modulated radiotherapy (IMRT)"),
      NA
    )
  }
)

test_that(
  "Trastuzumab, TS ONE, Cisplatin",
  {
    expect_equal(
      extract_drug_names("Trastuzumab, TS ONE, Cisplatin"),
      c("Trastuzumab", "TS ONE", "Cisplatin")
    )
  }
)

test_that(
  "MEDI-551 4 mg/kg",
  {
    expect_equal(
      extract_drug_names("MEDI-551 4 mg/kg"),
      "MEDI-551"
    )
  }
)

test_that(
  "Akt inhibitor MK2206",
  {
    expect_equal(
      extract_drug_names("Akt inhibitor MK2206"),
      "MK2206"
    )
  }
)

test_that(
  "Complete Resection - Surgery for tumor removal",
  {
    expect_equal(
      extract_drug_names("Complete Resection - Surgery for tumor removal"),
      NA
    )
  }
)

test_that(
  "Dendritic Cell (DC) Injections",
  {
    expect_equal(
      extract_drug_names("Dendritic Cell (DC) Injections"),
      NA
    )
  }
)

test_that(
  "Radiation Therapy",
  {
    expect_equal(
      extract_drug_names("Radiation Therapy"),
      NA
    )
  }
)

test_that(
  "Phase I: Glembatumumab Vedotin",
  {
    expect_equal(
      extract_drug_names("Phase I: Glembatumumab Vedotin"),
      "Glembatumumab Vedotin"
    )
  }
)

test_that(
  "E7389 28 Day Cycle",
  {
    expect_equal(
      extract_drug_names("E7389 28 Day Cycle"),
      "E7389"
    )
  }
)

test_that(
  "FOLFOXIRI Protocol",
  {
    expect_equal(
      extract_drug_names("FOLFOXIRI Protocol"),
      "FOLFOXIRI"
    )
  }
)

test_that(
  "Hypomethylating Agent (HMA)",
  {
    expect_equal(
      extract_drug_names("Hypomethylating Agent (HMA)"),
      NA
    )
  }
)

test_that(
  "gamma-secretase/Notch signalling pathway inhibitor RO4929097",
  {
    expect_equal(
      extract_drug_names("gamma-secretase/Notch signalling pathway inhibitor RO4929097"),
      "RO4929097"
    )
  }
)

test_that(
  "gamma-secretase/Notch signalling pathway inhibitor RO4929097",
  {
    expect_equal(
      extract_drug_names("gamma-secretase/Notch signalling pathway inhibitor RO4929097"),
      "RO4929097"
    )
  }
)

test_that(
  "PV-10 (10% rose bengal disodium)",
  {
    expect_equal(
      extract_drug_names("PV-10 (10% rose bengal disodium)"),
      c("PV-10", "rose bengal disodium")
    )
  }
)

test_that(
  "glembatumumab vedotin and PD-1 targeted checkpoint inhibitor (nivolumab OR pembrolizumab)",
  {
    expect_equal(
      extract_drug_names("glembatumumab vedotin and PD-1 targeted checkpoint inhibitor (nivolumab OR pembrolizumab)"),
      c("glembatumumab vedotin", "nivolumab", "pembrolizumab")
    )
  }
)

test_that(
  "Panitumumab 20 MG/ML Intravenous Solution [VECTIBIX]",
  {
    expect_equal(
      extract_drug_names("Panitumumab 20 MG/ML Intravenous Solution [VECTIBIX]"),
      c("Panitumumab", "VECTIBIX")
    )
  }
)

test_that(
  "OncoSec Medical System (OMS)",
  {
    expect_equal(
      extract_drug_names("OncoSec Medical System (OMS)"),
      NA
    )
  }
)

test_that(
  "ON 01910.Na",
  {
    expect_equal(
      extract_drug_names("ON 01910.Na"),
      "ON 01910.Na"
    )
  }
)

test_that(
  "INCB040093 Monotherapy",
  {
    expect_equal(
      extract_drug_names("INCB040093 Monotherapy"),
      "INCB040093"
    )
  }
)

test_that(
  "Mocetinostat - 50 mg",
  {
    expect_equal(
      extract_drug_names("Mocetinostat - 50 mg"),
      "Mocetinostat"
    )
  }
)

test_that(
  "Mocetinostat - Recommended Phase 2 Dose (70 mg)",
  {
    expect_equal(
      extract_drug_names("Mocetinostat - Recommended Phase 2 Dose (70 mg)"),
      "Mocetinostat"
    )
  }
)

test_that(
  "Arm 1: L19IL2 + Dacarbazine",
  {
    expect_equal(
      extract_drug_names("Arm 1: L19IL2 + Dacarbazine"),
      c("L19IL2", "Dacarbazine")
    )
  }
)

test_that(
  "Combination of two marketed drugs (irinotecan and cisplatin)",
  {
    expect_equal(
      extract_drug_names("Combination of two marketed drugs (irinotecan and cisplatin)"),
      c("irinotecan", "cisplatin")
    )
  }
)

test_that(
  "Pressurized IntraPeritoneal Air-flow Chemotherapy (PIPAC) fixed repeated dose",
  {
    expect_equal(
      extract_drug_names("Pressurized IntraPeritoneal Air-flow Chemotherapy (PIPAC) fixed repeated dose"),
      NA
    )
  }
)

test_that(
  "Bortezomib (drug)",
  {
    expect_equal(
      extract_drug_names("Bortezomib (drug)"),
      "Bortezomib"
    )
  }
)

test_that(
  "Vancomycin Oral Capsule",
  {
    expect_equal(
      extract_drug_names("Vancomycin Oral Capsule"),
      "Vancomycin"
    )
  }
)

test_that(
  "MEDI4736 in combination with nab-paclitaxel and gemcitabine",
  {
    expect_equal(
      extract_drug_names("MEDI4736 in combination with nab-paclitaxel and gemcitabine"),
      c("MEDI4736", "nab-paclitaxel", "gemcitabine")
    )
  }
)

test_that(
  "'SMT-NK' Inj (allogeneic Natural Killer cell)",
  {
    expect_equal(
      extract_drug_names("'SMT-NK' Inj (allogeneic Natural Killer cell)"),
      "SMT-NK"
    )
  }
)

test_that(
  "Nivolumab: Cohort 1 (Cutaneous)",
  {
    expect_equal(
      extract_drug_names("Nivolumab: Cohort 1 (Cutaneous)"),
      "Nivolumab"
    )
  }
)

test_that(
  "Phase Ib: Fisogatinib (BLU-554) 400mg in combination with Sugemalimab (CS1001) 1200mg",
  {
    expect_equal(
      extract_drug_names("Phase Ib: Fisogatinib (BLU-554) 400mg in combination with Sugemalimab (CS1001) 1200mg"),
      c("Fisogatinib", "BLU-554", "Sugemalimab", "CS1001")
    )
  }
)

test_that(
  "Cohort A: varlilumab & ipilimumab; Cohort B: varlilumab, ipilimumab, CDX-1401 & poly-ICLC",
  {
    expect_equal(
      extract_drug_names("Cohort A: varlilumab & ipilimumab; Cohort B: varlilumab, ipilimumab, CDX-1401 & poly-ICLC"),
      c("varlilumab", "ipilimumab", "CDX-1401", "poly-ICLC")
    )
  }
)

test_that(
  "Oral 5-Azacitidine 200 MG",
  {
    expect_equal(
      extract_drug_names("Oral 5-Azacitidine 200 MG"),
      "5-Azacitidine"
    )
  }
)

test_that(
  "Investigator's choice of platinum agent",
  {
    expect_equal(
      extract_drug_names("Investigator's choice of platinum agent"),
      NA
    )
  }
)

test_that(
  "GL-ONC1 alone, or in combination with chemotherapy with or without bevacizumab",
  {
    expect_equal(
      extract_drug_names("GL-ONC1 alone, or in combination with chemotherapy with or without bevacizumab"),
      c("GL-ONC1", "bevacizumab")
    )
  }
)

test_that(
  "Hsp90 inhibitor AUY922",
  {
    expect_equal(
      extract_drug_names("Hsp90 inhibitor AUY922"),
      "AUY922"
    )
  }
)

test_that(
  "GSK1363089 (formerly XL880)",
  {
    expect_equal(
      extract_drug_names("GSK1363089 (formerly XL880)"),
      c("GSK1363089", "XL880")
    )
  }
)

test_that(
  "IBI310 (anti-CTLA-4 antibody)",
  {
    expect_equal(
      extract_drug_names("IBI310 (anti-CTLA-4 antibody)"),
      "IBI310"
    )
  }
)

test_that(
  "Sintilimab(anti-PD-1 antibody)",
  {
    expect_equal(
      extract_drug_names("Sintilimab(anti-PD-1 antibody)"),
      "Sintilimab"
    )
  }
)

test_that(
  "Low Dose Fractionated Radiation Therapy",
  {
    expect_equal(
      extract_drug_names("Low Dose Fractionated Radiation Therapy"),
      NA
    )
  }
)

test_that(
  "IDO1 Inhibitor BMS-986205",
  {
    expect_equal(
      extract_drug_names("IDO1 Inhibitor BMS-986205"),
      "BMS-986205"
    )
  }
)

test_that(
  "FGFR Inhibitor AZD4547",
  {
    expect_equal(
      extract_drug_names("FGFR Inhibitor AZD4547"),
      "AZD4547"
    )
  }
)

test_that(
  "Sintilimab Combined With Docetaxel",
  {
    expect_equal(
      extract_drug_names("Sintilimab Combined With Docetaxel"),
      c("Sintilimab", "Docetaxel")
    )
  }
)

test_that(
  "Brentuximab Vedotin - induction",
  {
    expect_equal(
      extract_drug_names("Brentuximab Vedotin - induction"),
      "Brentuximab Vedotin"
    )
  }
)

test_that(
  "Modulated Electro-Hyperthermia (mEHT)",
  {
    expect_equal(
      extract_drug_names("Modulated Electro-Hyperthermia (mEHT)"),
      NA
    )
  }
)

test_that(
  "TCM Herbal Decoction (Shi Pi)",
  {
    expect_equal(
      extract_drug_names("TCM Herbal Decoction (Shi Pi)"),
      "Shi Pi"
    )
  }
)

test_that(
  "Arm A : Gemcitabine + Pemetrexed",
  {
    expect_equal(
      extract_drug_names("Arm A : Gemcitabine + Pemetrexed"),
      c("Gemcitabine", "Pemetrexed")
    )
  }
)

test_that(
  "EBV-specific T cells (EBV-CTLs)",
  {
    expect_equal(
      extract_drug_names("EBV-specific T cells (EBV-CTLs)"),
      NA
    )
  }
)

test_that(
  "ModraDoc006/r",
  {
    expect_equal(
      extract_drug_names("ModraDoc006/r"),
      "ModraDoc006"
    )
  }
)

test_that(
  "intrapleural docetaxel administration",
  {
    expect_equal(
      extract_drug_names("intrapleural docetaxel administration"),
      "docetaxel"
    )
  }
)

test_that(
  "Chromogenic in situ hybridization",
  {
    expect_equal(
      extract_drug_names("Chromogenic in situ hybridization"),
      NA
    )
  }
)

test_that(
  "Atezolizumab (MPDL3280A), an Engineered Anti-PD-L1 Antibody",
  {
    expect_equal(
      extract_drug_names("Atezolizumab (MPDL3280A), an Engineered Anti-PD-L1 Antibody"),
      c("Atezolizumab", "MPDL3280A")
    )
  }
)

test_that(
  "Autologous Stem Cell Transplant (ASCT)",
  {
    expect_equal(
      extract_drug_names("Autologous Stem Cell Transplant (ASCT)"),
      NA
    )
  }
)

test_that(
  "Anti-B7H1 Monoclonal Antibody MEDI4736",
  {
    expect_equal(
      extract_drug_names("Anti-B7H1 Monoclonal Antibody MEDI4736"),
      "MEDI4736"
    )
  }
)

test_that(
  "Pressurized IntraPeritoneal Air-flow Chemotherapy (PIPAC) increasing single dose",
  {
    expect_equal(
      extract_drug_names("Pressurized IntraPeritoneal Air-flow Chemotherapy (PIPAC) increasing single dose"),
      NA
    )
  }
)

test_that(
  "BIND-014 (docetaxel nanoparticles for injectable suspension)",
  {
    expect_equal(
      extract_drug_names("BIND-014 (docetaxel nanoparticles for injectable suspension)"),
      c("BIND-014", "docetaxel")
    )
  }
)

test_that(
  "Belantamab mafodotin lyophilized powder",
  {
    expect_equal(
      extract_drug_names("Belantamab mafodotin lyophilized powder"),
      "Belantamab mafodotin"
    )
  }
)

test_that(
  "I-DAC (Intermediate dose cytarabine)",
  {
    expect_equal(
      extract_drug_names("I-DAC (Intermediate dose cytarabine)"),
      c("I-DAC", "cytarabine")
    )
  }
)

test_that(
  "Paclitaxel 45 mg/m^2",
  {
    expect_equal(
      extract_drug_names("Paclitaxel 45 mg/m^2"),
      "Paclitaxel"
    )
  }
)

test_that(
  "Lenvatinib matching placebo",
  {
    expect_equal(
      extract_drug_names("Lenvatinib matching placebo"),
      "Lenvatinib"
    )
  }
)

test_that(
  "Questionnaire administration",
  {
    expect_equal(
      extract_drug_names("Questionnaire administration"),
      NA
    )
  }
)

test_that(
  "Quality-of-life assessment",
  {
    expect_equal(
      extract_drug_names("Quality-of-life assessment"),
      NA
    )
  }
)

test_that(
  "Omaveloxolone Capsules (2.5 mg/capsule)",
  {
    expect_equal(
      extract_drug_names("Omaveloxolone Capsules (2.5 mg/capsule)"),
      "Omaveloxolone"
    )
  }
)

test_that(
  "RomiDEPsin 10 MG/M2",
  {
    expect_equal(
      extract_drug_names("RomiDEPsin 10 MG/M2"),
      "RomiDEPsin"
    )
  }
)

test_that(
  "Combination of varlilumab and nivolumab",
  {
    expect_equal(
      extract_drug_names("Combination of varlilumab and nivolumab"),
      c("varlilumab", "nivolumab")
    )
  }
)

test_that(
  "CAR-CD19 T Cells",
  {
    expect_equal(
      extract_drug_names("CAR-CD19 T Cells"),
      NA
    )
  }
)

test_that(
  "MT-3724 Phase 1",
  {
    expect_equal(
      extract_drug_names("MT-3724 Phase 1"),
      "MT-3724"
    )
  }
)

test_that(
  "MT-3724 Phase 1",
  {
    expect_equal(
      extract_drug_names("MT-3724 Phase II"),
      "MT-3724"
    )
  }
)

test_that(
  "Lenalidomide and Gemcitabine (Dose level 11)",
  {
    expect_equal(
      extract_drug_names("Lenalidomide and Gemcitabine (Dose level 11)"),
      c("Lenalidomide", "Gemcitabine")
    )
  }
)

test_that(
  "Intervention C Daridorexant",
  {
    expect_equal(
      extract_drug_names("Intervention C Daridorexant"),
      "Daridorexant"
    )
  }
)

test_that(
  "Extended Consultation for apremilast",
  {
    expect_equal(
      extract_drug_names("Extended Consultation for apremilast"),
      "apremilast"
    )
  }
)

test_that(
  "Quantitative Sensory Test 2",
  {
    expect_equal(
      extract_drug_names("Quantitative Sensory Test 2"),
      NA
    )
  }
)

test_that(
  "Prednisone tablet",
  {
    expect_equal(
      extract_drug_names("Prednisone tablet"),
      "Prednisone"
    )
  }
)

test_that(
  "Heart rate synchronized motor imagery music",
  {
    expect_equal(
      extract_drug_names("Heart rate synchronized motor imagery music"),
      NA
    )
  }
)

test_that(
  "LNA043 Dosing Regimen A",
  {
    expect_equal(
      extract_drug_names("LNA043 Dosing Regimen A"),
      "LNA043"
    )
  }
)

test_that(
  "Middle meningeal artery embolization",
  {
    expect_equal(
      extract_drug_names("Middle meningeal artery embolization"),
      NA
    )
  }
)

test_that(
  "Standard Dose (SD) RSVt vaccine",
  {
    expect_equal(
      extract_drug_names("Standard Dose (SD) RSVt vaccine"),
      NA
    )
  }
)

test_that(
  "LUM 2.6 Imaging Device",
  {
    expect_equal(
      extract_drug_names("LUM 2.6 Imaging Device"),
      NA
    )
  }
)

test_that(
  "Conventional Physiotherapy",
  {
    expect_equal(
      extract_drug_names("Conventional Physiotherapy"),
      NA
    )
  }
)

test_that(
  "Clinical decisions support",
  {
    expect_equal(
      extract_drug_names("Clinical decisions support"),
      NA
    )
  }
)

test_that(
  "Pramipexole 0.088mg/tid",
  {
    expect_equal(
      extract_drug_names("Pramipexole 0.088mg/tid"),
      "Pramipexole"
    )
  }
)

test_that(
  "Evaluation of circulating donor free DNA in the recipient's blood (Dd-cfDNA: donor-derived cell free DNA)",
  {
    expect_equal(
      extract_drug_names("Evaluation of circulating donor free DNA in the recipient's blood (Dd-cfDNA: donor-derived cell free DNA)"),
      NA
    )
  }
)

test_that(
  "Mandibular advancement Device",
  {
    expect_equal(
      extract_drug_names("Mandibular advancement Device"),
      NA
    )
  }
)

test_that(
  "Enhanced usual care (EUC)",
  {
    expect_equal(
      extract_drug_names("Enhanced usual care (EUC)"),
      NA
    )
  }
)

test_that(
  "Wedge 1",
  {
    expect_equal(
      extract_drug_names("Wedge 1"),
      NA
    )
  }
)

test_that(
  "Positron Emission Tomography",
  {
    expect_equal(
      extract_drug_names("Positron Emission Tomography"),
      NA
    )
  }
)

test_that(
  "Web-based Aerobic Exercise Program",
  {
    expect_equal(
      extract_drug_names("Web-based Aerobic Exercise Program"),
      NA
    )
  }
)

test_that(
  "My Pelvic Plan website",
  {
    expect_equal(
      extract_drug_names("My Pelvic Plan website"),
      NA
    )
  }
)

test_that(
  "TTYP01 single ascending doses",
  {
    expect_equal(
      extract_drug_names("TTYP01 single ascending doses"),
      "TTYP01"
    )
  }
)

test_that(
  "TTYP01, up to 120 mg",
  {
    expect_equal(
      extract_drug_names("TTYP01, up to 120 mg"),
      "TTYP01"
    )
  }
)

test_that(
  "Commercially available cigarette",
  {
    expect_equal(
      extract_drug_names("Commercially available cigarette"),
      NA
    )
  }
)

test_that(
  "Cooking Intervention",
  {
    expect_equal(
      extract_drug_names("Dual then single task situation"),
      NA
    )
  }
)

test_that(
  "Partial mastectomy plus additional Shave Margin",
  {
    expect_equal(
      extract_drug_names("Partial mastectomy plus additional Shave Margin"),
      NA
    )
  }
)

test_that(
  "0.9% sodium chloride",
  {
    expect_equal(
      extract_drug_names("0.9% sodium chloride"),
      "sodium chloride"
    )
  }
)

test_that(
  "Substance Use and Health Risk Intervention (SUHRI)",
  {
    expect_equal(
      extract_drug_names("Substance Use and Health Risk Intervention (SUHRI)"),
      NA
    )
  }
)

test_that(
  "Pseudoword learning paradigm task",
  {
    expect_equal(
      extract_drug_names("Pseudoword learning paradigm task"),
      NA
    )
  }
)

test_that(
  "Exercise",
  {
    expect_equal(
      extract_drug_names("Exercise"),
      NA
    )
  }
)

test_that(
  "Topical Diclofenac",
  {
    expect_equal(
      extract_drug_names("Topical Diclofenac"),
      "Diclofenac"
    )
  }
)

test_that(
  "Platelet-Rich-Plasma (PRP)",
  {
    expect_equal(
      extract_drug_names("Platelet-Rich-Plasma (PRP)"),
      NA
    )
  }
)

test_that(
  "Waiting period with usual care",
  {
    expect_equal(
      extract_drug_names("Waiting period with usual care"),
      NA
    )
  }
)

test_that(
  "Dietary counseling",
  {
    expect_equal(
      extract_drug_names("Dietary counseling"),
      NA
    )
  }
)

test_that(
  "Guideline Directed Medical Therapy for Heart Failure (GDMT)",
  {
    expect_equal(
      extract_drug_names("Guideline Directed Medical Therapy for Heart Failure (GDMT)"),
      NA
    )
  }
)

test_that(
  "Second Placebo",
  {
    expect_equal(
      extract_drug_names("Second Placebo"),
      NA
    )
  }
)

test_that(
  "Second Placebo",
  {
    expect_equal(
      extract_drug_names("Second Placebo"),
      NA
    )
  }
)

test_that(
  "active sham stimulation",
  {
    expect_equal(
      extract_drug_names("active sham stimulation"),
      NA
    )
  }
)

test_that(
  "Discontinuation of antiplatelet agent group",
  {
    expect_equal(
      extract_drug_names("Discontinuation of antiplatelet agent group"),
      NA
    )
  }
)

test_that(
  "Compound heating strategy",
  {
    expect_equal(
      extract_drug_names("Compound heating strategy"),
      NA
    )
  }
)

test_that(
  "Interactive film",
  {
    expect_equal(
      extract_drug_names("Interactive film"),
      NA
    )
  }
)

test_that(
  "Nurse Counseling Based on the Chronic Care Model (CCM)",
  {
    expect_equal(
      extract_drug_names("Nurse Counseling Based on the Chronic Care Model (CCM)"),
      NA
    )
  }
)

test_that(
  "Web-based diabetic foot education based on health promotion model",
  {
    expect_equal(
      extract_drug_names("Web-based diabetic foot education based on health promotion model"),
      NA
    )
  }
)

test_that(
  "Face to face diabetic foot education based on health promotion model",
  {
    expect_equal(
      extract_drug_names("Face to face diabetic foot education based on health promotion model"),
      NA
    )
  }
)

test_that(
  "Cow's milk",
  {
    expect_equal(
      extract_drug_names("Cow's milk"),
      NA
    )
  }
)

test_that(
  "Oat drink",
  {
    expect_equal(
      extract_drug_names("Oat drink"),
      NA
    )
  }
)

test_that(
  "Inactive Placebo Oral Capsule",
  {
    expect_equal(
      extract_drug_names("Inactive Placebo Oral Capsule"),
      NA
    )
  }
)

test_that(
  "Cognitive Behavioral Therapy (CBT)",
  {
    expect_equal(
      extract_drug_names("Cognitive Behavioral Therapy (CBT)"),
      NA
    )
  }
)

test_that(
  "Functional magnetic resonance imaging (fMRI)",
  {
    expect_equal(
      extract_drug_names("Functional magnetic resonance imaging (fMRI)"),
      NA
    )
  }
)

test_that(
  "Proton magnetic resonance spectroscopy (MRS) Imaging",
  {
    expect_equal(
      extract_drug_names("Proton magnetic resonance spectroscopy (MRS) Imaging"),
      NA
    )
  }
)

test_that(
  "Normal Education Material",
  {
    expect_equal(
      extract_drug_names("Normal Education Material"),
      NA
    )
  }
)

test_that(
  "Low-fat meal",
  {
    expect_equal(
      extract_drug_names("Low-fat meal"),
      NA
    )
  }
)

test_that(
  "Thai Foot Massage",
  {
    expect_equal(
      extract_drug_names("Low-fat meal"),
      NA
    )
  }
)

test_that(
  "18F FDG PET/CT",
  {
    expect_equal(
      extract_drug_names("18F FDG PET/CT"),
      NA
    )
  }
)

test_that(
  "Vicryl-plus, monocryl-plus, PDS-plus (Triclosan-coated Sutures)",
  {
    expect_equal(
      extract_drug_names("Vicryl-plus, monocryl-plus, PDS-plus (Triclosan-coated Sutures)"),
      NA
    )
  }
)

test_that(
  "Vicryl, monocryl, PDS (not coated with triclosan)",
  {
    expect_equal(
      extract_drug_names("Vicryl, monocryl, PDS (not coated with triclosan)"),
      NA
    )
  }
)

test_that(
  "anti-CD19 CAR-T cells",
  {
    expect_equal(
      extract_drug_names("anti-CD19 CAR-T cells"),
      NA
    )
  }
)

test_that(
  "Autologous Plasmablasts (B cells)",
  {
    expect_equal(
      extract_drug_names("Autologous Plasmablasts (B cells)"),
      NA
    )
  }
)

test_that(
  "Croq'Santé nutrition education program",
  {
    expect_equal(
      extract_drug_names("Croq'Santé nutrition education program"),
      NA
    )
  }
)

test_that(
  "Nicotine Replacement Product",
  {
    expect_equal(
      extract_drug_names("Nicotine Replacement Product"),
      NA
    )
  }
)

test_that(
  "Abdominal Massage",
  {
    expect_equal(
      extract_drug_names("Abdominal Massage"),
      NA
    )
  }
)

test_that(
  "Patient Education",
  {
    expect_equal(
      extract_drug_names("Patient Education"),
      NA
    )
  }
)

test_that(
  "Demineralized Dentin Allograft",
  {
    expect_equal(
      extract_drug_names("Demineralized Dentin Allograft"),
      NA
    )
  }
)

test_that(
  "Smoking cessation",
  {
    expect_equal(
      extract_drug_names("Smoking cessation"),
      NA
    )
  }
)

test_that(
  "Statin (such as Simvastatin, Atorvastatin, Rosuvastatin, Pravastatin)",
  {
    expect_equal(
      extract_drug_names("Statin (such as Simvastatin, Atorvastatin, Rosuvastatin, Pravastatin)"),
      c("Simvastatin", "Atorvastatin", "Rosuvastatin", "Pravastatin")
    )
  }
)

test_that(
  "A mental health art exhibition",
  {
    expect_equal(
      extract_drug_names("A mental health art exhibition"),
      NA
    )
  }
)

test_that(
  "printed or online education materials",
  {
    expect_equal(
      extract_drug_names("printed or online education materials"),
      NA
    )
  }
)

test_that(
  "Non-culprit-lesion revascularization",
  {
    expect_equal(
      extract_drug_names("Non-culprit-lesion revascularization"),
      NA
    )
  }
)

test_that(
  "Efgartigimod PH20 SC - prefilled syringe",
  {
    expect_equal(
      extract_drug_names("Efgartigimod PH20 SC - prefilled syringe"),
      "Efgartigimod PH20 SC"
    )
  }
)

test_that(
  "Continuous Nebulization of Salbutamol",
  {
    expect_equal(
      extract_drug_names("Continuous Nebulization of Salbutamol"),
      "Salbutamol"
    )
  }
)

test_that(
  "Dyad Plus",
  {
    expect_equal(
      extract_drug_names("Dyad Plus"),
      "Dyad Plus"
    )
  }
)

test_that(
  "Reminder Focused Positive Psychiatry",
  {
    expect_equal(
      extract_drug_names("Reminder Focused Positive Psychiatry"),
      NA
    )
  }
)

test_that(
  "3D Kidney model",
  {
    expect_equal(
      extract_drug_names("3D Kidney model"),
      NA
    )
  }
)

test_that(
  "BAY2757556 (Larotrectinib, Vitrakvi)",
  {
    expect_equal(
      extract_drug_names("BAY2757556 (Larotrectinib, Vitrakvi)"),
      c("BAY2757556", "Larotrectinib", "Vitrakvi")
    )
  }
)

test_that(
  "Open-Label Extension PLS240",
  {
    expect_equal(
      extract_drug_names("Open-Label Extension PLS240"),
      "PLS240"
    )
  }
)

test_that(
  "sham High energy density pulse electromagnetic field",
  {
    expect_equal(
      extract_drug_names("sham High energy density pulse electromagnetic field"),
      NA
    )
  }
)

test_that(
  "Placebo matching BI 1815368",
  {
    expect_equal(
      extract_drug_names("Placebo matching BI 1815368"),
      "BI 1815368"
    )
  }
)

test_that(
  "Placebo Low Level Laser Therapy",
  {
    expect_equal(
      extract_drug_names("Placebo Low Level Laser Therapy"),
      NA
    )
  }
)

test_that(
  "MyMammogram decision aid",
  {
    expect_equal(
      extract_drug_names("MyMammogram decision aid"),
      NA
    )
  }
)

test_that(
  "Midazolam 2.5 mg oromucosal solution",
  {
    expect_equal(
      extract_drug_names("Midazolam 2.5 mg oromucosal solution"),
      "Midazolam"
    )
  }
)

test_that(
  "Atezolizumab 1200 mg e.v. q21",
  {
    expect_equal(
      extract_drug_names("Atezolizumab 1200 mg e.v. q21"),
      "Atezolizumab"
    )
  }
)

test_that(
  "Tadalafil 5 mg tablets with a digital tool.",
  {
    expect_equal(
      extract_drug_names("Tadalafil 5 mg tablets with a digital tool."),
      "Tadalafil"
    )
  }
)

test_that(
  "In-Person Mindfulness-based Group Sessions",
  {
    expect_equal(
      extract_drug_names("In-Person Mindfulness-based Group Sessions"),
      NA
    )
  }
)

test_that(
  "Subject's Current Diabetes Therapy",
  {
    expect_equal(
      extract_drug_names("Subject's Current Diabetes Therapy"),
      NA
    )
  }
)

test_that(
  "Non-surgical periodontal treatment",
  {
    expect_equal(
      extract_drug_names("Non-surgical periodontal treatment"),
      NA
    )
  }
)

test_that(
  "Subjective assessments",
  {
    expect_equal(
      extract_drug_names("Subjective assessments"),
      NA
    )
  }
)

test_that(
  "ABBV-CLS-7262 Dose 1",
  {
    expect_equal(
      extract_drug_names("ABBV-CLS-7262 Dose 1"),
      "ABBV-CLS-7262"
    )
  }
)

test_that(
  "Flu Seasonal /SARS-CoV-2 mRNA Dose 1",
  {
    expect_equal(
      extract_drug_names("Flu Seasonal /SARS-CoV-2 mRNA Dose 1"),
      NA
    )
  }
)

test_that(
  "Phase 2 selected Investigational Flu Seasonal/SARS-CoV-2 mRNA",
  {
    expect_equal(
      extract_drug_names("Phase 2 selected Investigational Flu Seasonal/SARS-CoV-2 mRNA"),
      NA
    )
  }
)

test_that(
  "Superior laryngeal nerve block - Intervention (Bupivacaine and triamcinolone acetonide suspension)",
  {
    expect_equal(
      extract_drug_names("Superior laryngeal nerve block - Intervention (Bupivacaine and triamcinolone acetonide suspension)"),
      c("Bupivacaine", "triamcinolone acetonide")
    )
  }
)

test_that(
  "Mobile Application with Interactive Nurse Support",
  {
    expect_equal(
      extract_drug_names("Mobile Application with Interactive Nurse Support"),
      NA
    )
  }
)

test_that(
  "Mobile Health (mHealth) Patient-Reported Outcome (PRO) tool",
  {
    expect_equal(
      extract_drug_names("Mobile Health (mHealth) Patient-Reported Outcome (PRO) tool"),
      NA
    )
  }
)

test_that(
  "6-weeks CBT smoking cessation programme",
  {
    expect_equal(
      extract_drug_names("6-weeks CBT smoking cessation programme"),
      NA
    )
  }
)

test_that(
  "Long-term drug therapy within a therapeutic community",
  {
    expect_equal(
      extract_drug_names("Long-term drug therapy within a therapeutic community"),
      NA
    )
  }
)

test_that(
  "lumbosacral transforaminal epidural injection at the level of L4-5, L5-S1, or S1 neural foramina",
  {
    expect_equal(
      extract_drug_names("lumbosacral transforaminal epidural injection at the level of L4-5, L5-S1, or S1 neural foramina"),
      NA
    )
  }
)

test_that(
  "APrevent® VOIS-Implant",
  {
    expect_equal(
      extract_drug_names("APrevent® VOIS-Implant"),
      NA
    )
  }
)

test_that(
  "CBD Day 1",
  {
    expect_equal(
      extract_drug_names("CBD Day 1"),
      "CBD"
    )
  }
)

test_that(
  "Bevacizumab or Bevacizumab biosimilar",
  {
    expect_equal(
      extract_drug_names("Bevacizumab or Bevacizumab biosimilar"),
      "Bevacizumab"
    )
  }
)

test_that(
  "Placebo matched to ocrelizumab",
  {
    expect_equal(
      extract_drug_names("Placebo matched to ocrelizumab"),
      "ocrelizumab"
    )
  }
)

test_that(
  "PS setting strategy in pressure-support ventilated patients",
  {
    expect_equal(
      extract_drug_names("PS setting strategy in pressure-support ventilated patients"),
      NA
    )
  }
)

test_that(
  "cGMP-AAs (20 g of protein equivalent) + 50 g of low-protein bread + 160 g of unpeeled apple",
  {
    expect_equal(
      extract_drug_names("cGMP-AAs (20 g of protein equivalent) + 50 g of low-protein bread + 160 g of unpeeled apple"),
      NA
    )
  }
)

test_that(
  "PBI Radiotherapy 6 Gy",
  {
    expect_equal(
      extract_drug_names("PBI Radiotherapy 6 Gy"),
      NA
    )
  }
)

test_that(
  "CNTO 2476 (6.0 * 10^4 cells)",
  {
    expect_equal(
      extract_drug_names("CNTO 2476 (6.0 * 10^4 cells)"),
      "CNTO 2476"
    )
  }
)

test_that(
  "1:1 ratio for Treatment Group and Control Group",
  {
    expect_equal(
      extract_drug_names("1:1 ratio for Treatment Group and Control Group"),
      NA
    )
  }
)

test_that(
  "JNJ-42756493 Prototype Formulation I (G-025)",
  {
    expect_equal(
      extract_drug_names("JNJ-42756493 Prototype Formulation I (G-025)"),
      c("JNJ-42756493", "G-025")
    )
  }
)

test_that(
  "Remimazolam besylate and low-dose Esketamine",
  {
    expect_equal(
      extract_drug_names("Remimazolam besylate and low-dose Esketamine"),
      c("Remimazolam besylate", "Esketamine")
    )
  }
)

test_that(
  "Remimazolam besylate and Medium dose Esketamine",
  {
    expect_equal(
      extract_drug_names("Remimazolam besylate and Medium dose Esketamine"),
      c("Remimazolam besylate", "Esketamine")
    )
  }
)

test_that(
  "Mono drug group of Rosuvastatin 20 mg",
  {
    expect_equal(
      extract_drug_names("Mono drug group of Rosuvastatin 20 mg"),
      "Rosuvastatin"
    )
  }
)

test_that(
  "Mobile health app",
  {
    expect_equal(
      extract_drug_names("Mobile health app"),
      NA
    )
  }
)

test_that(
  "Culturally tailored resilience-building intervention",
  {
    expect_equal(
      extract_drug_names("Culturally tailored resilience-building intervention"),
      NA
    )
  }
)

test_that(
  "Low Level Light Therapy",
  {
    expect_equal(
      extract_drug_names("Low Level Light Therapy"),
      NA
    )
  }
)

test_that(
  "IV Glofitamab",
  {
    expect_equal(
      extract_drug_names("IV Glofitamab"),
      "Glofitamab"
    )
  }
)

test_that(
  "0.1ml/kg normal saline",
  {
    expect_equal(
      extract_drug_names("0.1ml/kg normal saline"),
      "normal saline"
    )
  }
)

test_that(
  "Study Visit 1",
  {
    expect_equal(
      extract_drug_names("Study Visit 1"),
      NA
    )
  }
)

test_that(
  "Radiation of 45 Grays on 5 weeks",
  {
    expect_equal(
      extract_drug_names("Radiation of 45 Grays on 5 weeks"),
      NA
    )
  }
)

test_that(
  "Tocilizumab treatment",
  {
    expect_equal(
      extract_drug_names("Tocilizumab treatment"),
      "Tocilizumab"
    )
  }
)

test_that(
  "Paclitaxel + Carboplatin every 3 weeks",
  {
    expect_equal(
      extract_drug_names("Paclitaxel + Carboplatin every 3 weeks"),
      c("Paclitaxel", "Carboplatin")
    )
  }
)

test_that(
  "Paclitaxel + Carboplatin every 3 weeks",
  {
    expect_equal(
      extract_drug_names("Paclitaxel + Carboplatin every 3 weeks"),
      c("Paclitaxel", "Carboplatin")
    )
  }
)

test_that(
  "nab-paclitaxel IV",
  {
    expect_equal(
      extract_drug_names("nab-paclitaxel IV"),
      c("nab-paclitaxel")
    )
  }
)

test_that(
  "PharmaJet Tropis® device",
  {
    expect_equal(
      extract_drug_names("PharmaJet Tropis® device"),
      NA
    )
  }
)

test_that(
  "Placebo of aprepitant",
  {
    expect_equal(
      extract_drug_names("Placebo of aprepitant"),
      "aprepitant"
    )
  }
)

test_that(
  "Topical resiquimod 0.06%",
  {
    expect_equal(
      extract_drug_names("Topical resiquimod 0.06%"),
      "resiquimod"
    )
  }
)

test_that(
  "KN026 30 mg/kg Q3W",
  {
    expect_equal(
      extract_drug_names("KN026 30 mg/kg Q3W"),
      "KN026"
    )
  }
)
