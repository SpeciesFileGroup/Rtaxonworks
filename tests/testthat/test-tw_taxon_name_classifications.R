skip_on_cran()

test_that("taxon_name_classifications", {
  vcr::use_cassette("taxon_name_classifications", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_name_classifications(page=0, per=1)
  })

  expect_equal(strsplit(x$type[1], "::")[[1]][1], 'TaxonNameClassification')
})
