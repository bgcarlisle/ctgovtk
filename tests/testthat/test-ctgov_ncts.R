test_that("NCT search works", {
    json <- ctgov_ncts("NCT03430843")
    expect_equal(
        json$studies[[1]]$protocolSection$identificationModule$officialTitle,
        "A Randomized, Controlled, Open-label, Global Phase 3 Study Comparing the Efficacy of the Anti-PD-1 Antibody Tislelizumab (BGB-A317) Versus Chemotherapy as Second Line Treatment in Patients With Advanced Unresectable/Metastatic Esophageal Squamous Cell Carcinoma"
    )
})
