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
      c("ADI-PEG 20", "modified FOLFOX6")
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
      c("varlilumab", "ipilimumab", "varlilumab", "ipilimumab", "CDX-1401", "poly-ICLC")
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

