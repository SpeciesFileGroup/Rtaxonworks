skip_on_cran()

test_that("tw_taxon_names", {
  vcr::use_cassette("tw_taxon_names", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(page=0, per=1)
  })

  expect_equal(x$name[1], 'Root')
})
