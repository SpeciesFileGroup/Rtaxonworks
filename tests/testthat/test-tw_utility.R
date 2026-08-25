skip_on_cran()

test_that("tw_stats", {
  vcr::use_cassette("tw_stats", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    x <- tw_stats()
  })
  expect_true(is.list(x))
  expect_true("data" %in% names(x))
  expect_true(all(c("data", "metadata") %in% names(x$data)))
})

test_that("tw_count_valid_species", {
  vcr::use_cassette("tw_count_valid_species", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    x <- tw_count_valid_species(taxon_name = "Aphrodinae")
  })
  expect_true(is.list(x))
  expect_true("data" %in% names(x))
})
