skip_on_cran()

test_that("tw_taxon_determinations", {
  vcr::use_cassette("tw_taxon_determinations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_determinations(page = 0, per = 1)
  })
  expect_true(is.list(x))
  expect_true("data" %in% names(x))
  if (nrow(x$data) > 0) {
    expect_equal(x$data$base_class[1], "TaxonDetermination")
  }
})
