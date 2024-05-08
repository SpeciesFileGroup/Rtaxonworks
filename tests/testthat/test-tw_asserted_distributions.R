skip_on_cran()

test_that("tw_asserted_distributions", {
  vcr::use_cassette("tw_asserted_distributions", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_asserted_distributions(page = 0, per = 1)$data
  })

  expect_true(grepl("AssertedDistribution", x$global_id[1]))
  expect_equal(nrow(x), 1)
})
