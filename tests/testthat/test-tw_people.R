skip_on_cran()

test_that("tw_people_last_name", {
  vcr::use_cassette("tw_people_last_name", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_people(last_name = "Smith", page = 0, per = 1)$data
  })

  expect_equal(x$last_name[1], "Smith")
  expect_equal(nrow(x), 1)
})


test_that("tw_people_first_name", {
  vcr::use_cassette("tw_people_first_name", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_people(first_name = "Janet", page = 0, per = 1)$data
  })

  expect_equal(x$first_name[1], "Janet")
  expect_equal(nrow(x), 1)
})


test_that("tw_people_first_last_name", {
  vcr::use_cassette("tw_people_first_last_name", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_people(first_name = "E. L.", last_name = "Mockford", role = "SourceAuthor", 
    page = 0, per = 1)$data
  })
  expect_equal(x$last_name[1], "Mockford")
  expect_equal(x$first_name[1], "E. L.")
  expect_equal(nrow(x), 1)
})
