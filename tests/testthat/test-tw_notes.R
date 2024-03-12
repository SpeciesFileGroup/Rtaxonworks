skip_on_cran()

test_that("tw_notes", {
  vcr::use_cassette("tw_notes", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_notes(page = 0, per = 1)$data
  })

  expect_true("note_object_id" %in% colnames(x))
  expect_equal(nrow(x), 1)
})
