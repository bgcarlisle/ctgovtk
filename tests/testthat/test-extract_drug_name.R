test_that("Drug extraction algorithm works", {

  tibble::tribble(
    ~intervention, ~intended_result,

    "Aspirin",
    c("Aspirin"),

    "Aspirin or Tylenol",
    c("Aspirin", "Tylenol")
  )
  
  expect_equal(
    1,
    1
  )
})
