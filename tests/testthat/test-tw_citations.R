skip_on_cran()

test_that("tw_citations", {
  vcr::use_cassette("tw_citations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_citations(page = 0, per = 1)$data
  })

  expect_true("citation_object_type" %in% colnames(x))
  expect_equal(nrow(x), 1)
})
