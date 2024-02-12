skip_on_cran()

test_that("tw_biological_associations", {
  vcr::use_cassette("tw_biological_associations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations()
  })
  expect_equal(x$base_class[1], 'BiologicalAssociation')
})
