skip_on_cran()

test_that("tw_both_authenticated", {
  vcr::use_cassette("tw_both_authenticated", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_both_authenticated()$data
  })
  expect_true(all(as.logical(x$success)))
})

test_that("tw_both_authenticated_fail", {
  expect_error({
    vcr::use_cassette("tw_both_authenticated_fail", {
      assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
      assign("TW_PROJECT_TOKEN", "invalid", envir = .GlobalEnv)
      assign("TW_USER_TOKEN", "invalid", envir = .GlobalEnv)
      x <- tw_both_authenticated()$data
    })
  }, message = "HTTP 401 Unauthorized.")
})


test_that("tw_user_authenticated", {
  vcr::use_cassette("tw_user_authenticated", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_user_authenticated()$data
  })
  expect_true(all(as.logical(x$success)))
})

test_that("tw_user_authenticated_fail", {
  expect_error({
    vcr::use_cassette("tw_user_authenticated_fail", {
      assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
      assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
      assign("TW_USER_TOKEN", "invalid", envir = .GlobalEnv)
      x <- tw_user_authenticated()$data
    })
  }, message = "HTTP 401 Unauthorized.")
})

test_that("tw_project_authenticated", {
  vcr::use_cassette("tw_project_authenticated", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_project_authenticated()$data
  })
  expect_true(all(as.logical(x$success)))
})

test_that("tw_project_authenticated_fail", {
  expect_error({
    vcr::use_cassette("tw_project_authenticated_fail", {
      assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
      assign("TW_PROJECT_TOKEN", "invalid", envir = .GlobalEnv)
      assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
      x <- tw_project_authenticated()$data
    })
  }, message = "HTTP 401 Unauthorized.")
})
