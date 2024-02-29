skip_on_cran()

test_that("tw_collection_objects", {
  vcr::use_cassette("tw_collection_objects", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_co(collection_object_type='Specimen', page=0, per=1)
  })
  expect_equal(x$type[1], 'Specimen')
})
