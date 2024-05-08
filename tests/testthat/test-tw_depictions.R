skip_on_cran()

test_that("tw_depictions", {
  vcr::use_cassette("tw_depictions", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_depictions(page = 2, per = 1)$data
  })
  expect_true("depiction_object_type" %in% names(x))
})

test_that("tw_depictions_depiction_id", {
  vcr::use_cassette("tw_depictions_depiction_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    depiction_id <- tw_depictions(page = 1, per = 2)$data$id
    x <- tw_depictions(depiction_id = depiction_id, page = 0, per = 10)$data
  })
  expect_true(all(x$id %in% depiction_id))
})

test_that("tw_depictions_depiction_object_id", {
  vcr::use_cassette("tw_depictions_depiction_object_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    depiction_object_id <- tw_depictions()$data$depiction_object_id[1]
    x <- tw_depictions(depiction_object_id = depiction_object_id)$data
  })
  expect_equal(x$depiction_object_id, depiction_object_id)
})

test_that("tw_depictions_depiction_object_type", {
  vcr::use_cassette("tw_depictions_depiction_object_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    depiction_object_type <- tw_depictions()$data$depiction_object_type[1]
    x <- tw_depictions(depiction_object_type = depiction_object_type)$data
  })
  expect_true(all(x$depiction_object_type %in% depiction_object_type))
})

test_that("tw_depictions_image_id", {
  vcr::use_cassette("tw_depictions_image_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    image_id <- tw_depictions()$data$image_id[1]
    x <- tw_depictions(image_id = image_id, page = 0, per = 1)$data
  })
  expect_equal(x$image_id[1], image_id)
})
