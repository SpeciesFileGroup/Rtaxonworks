skip_on_cran()

# TODO: no pagination headers and no params?
test_that("tw_biological_relationships", {
  vcr::use_cassette("tw_biological_relationships", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_relationships(page = 0, per = 1)$data
  })

  expect_true("inverted_name" %in% colnames(x))
  expect_equal(nrow(x), 1)
})
