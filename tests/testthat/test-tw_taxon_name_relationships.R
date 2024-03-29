skip_on_cran()

test_that("tw_taxon_name_relationships", {
  vcr::use_cassette("tw_taxon_name_relationships", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_name_relationships(page = 0, per = 1)$data
  })

  expect_equal(strsplit(x$type[1], "::")[[1]][1], 'TaxonNameRelationship')
})
