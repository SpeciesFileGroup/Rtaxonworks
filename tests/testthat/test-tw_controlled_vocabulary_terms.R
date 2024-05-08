skip_on_cran()

test_that("tw_controlled_vocabulary_terms", {
  vcr::use_cassette("tw_controlled_vocabulary_terms", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_controlled_vocabulary_terms(page = 0, per = 1)$data
  })

  expect_true("css_color" %in% names(x))
  expect_equal(nrow(x), 1)
})

test_that("tw_controlled_vocabulary_terms_type", {
  vcr::use_cassette("tw_controlled_vocabulary_terms_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_controlled_vocabulary_terms(type = "BiologicalProperty", page = 0, per = 10)$data
  })

  expect_true(all(x$type == "BiologicalProperty"))
})