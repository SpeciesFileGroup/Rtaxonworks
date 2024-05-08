skip_on_cran()

test_that("tw_dwc_occurrences", {
  vcr::use_cassette("tw_dwc_occurrences", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_dwc_occurrences(page = 2, per = 1)$data
  })
  expect_true("basisOfRecord" %in% names(x))
})

test_that("tw_dwc_occurrences_dwc_occurrence_id", {
  vcr::use_cassette("tw_dwc_occurrences_dwc_occurrence_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    dwc_occurrence_id <- tw_dwc_occurrences(page = 2, per = 1)$data$id[1]
    x <- tw_dwc_occurrences(dwc_occurrence_id = dwc_occurrence_id, page = 0, per = 10)$data
  })
  expect_true(all(x$id %in% dwc_occurrence_id))
})

# TODO: filtering on dwc_occurrence_object_id doesn't work?
# test_that("tw_dwc_occurrences_dwc_occurrence_object_id", {
#   vcr::use_cassette("tw_dwc_occurrences_dwc_occurrence_object_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     dwc_occurrence_object_id <- tw_dwc_occurrences(page = 2, per = 1)$data$dwc_occurrence_object_id[1]
#     x <- tw_dwc_occurrences(dwc_occurrence_object_id = dwc_occurrence_object_id, page = 0, per = 10)$data
#   })
#   expect_equal(x$dwc_occurrence_object_id[1], dwc_occurrence_object_id)
# })

test_that("tw_dwc_occurrences_dwc_occurrence_object_type", {
  vcr::use_cassette("tw_dwc_occurrences_dwc_occurrence_object_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    dwc_occurrence_object_type <- tw_dwc_occurrences(page = 2, per = 1)$data$dwc_occurrence_object_type[1]
    x <- tw_dwc_occurrences(dwc_occurrence_object_type = dwc_occurrence_object_type, page = 0, per = 10)$data
  })
  expect_equal(x$dwc_occurrence_object_type[1], dwc_occurrence_object_type)
})