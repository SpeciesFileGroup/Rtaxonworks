skip_on_cran()

test_that("tw_collecting_events", {
  vcr::use_cassette("tw_collecting_events", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_collecting_events(page = 0, per = 1)$data
  })
  expect_true("verbatim_label" %in% names(x))
})
