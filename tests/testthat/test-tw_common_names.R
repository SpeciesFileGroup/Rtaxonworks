skip_on_cran()

test_that("tw_common_names", {
  vcr::use_cassette("tw_common_names", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_common_names(page = 0, per = 1)$data
  })

  expect_true("language" %in% colnames(x))
  expect_equal(nrow(x), 1)
})

# TODO: does not work in API
# test_that("tw_common_names_common_name_id", {
#   vcr::use_cassette("tw_common_names_common_name_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     common_name_id <- tw_common_names(page = 5, per = 3)$data$id
#     x <- tw_common_names(common_name_id = common_name_id, page = 0, per = 10)$data
#   })
#   expect_true(all(x$id %in% common_name_id))
#   expect_true(nrow(x) == 3)
# })

# TODO: no example data
# test_that("tw_common_names_geographic_area_id", {
#   vcr::use_cassette("tw_common_names_geographic_area_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     geographic_area_id <- tw_common_names(page = 5, per = 1)$data$geographic_area_id[1]
#     x <- tw_common_names(geographic_area_id = geographic_area_id, page = 0, per = 10)$data
#   })
#   expect_true(all(x$geographic_area_id == geographic_area_id))
# })

test_that("tw_common_names_language_id", {
  vcr::use_cassette("tw_common_names_language_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    language_id <- tw_common_names(page = 1, per = 100)$data$language_id[1]
    language_id <- language_id[!is.na(language_id)][1]
    x <- tw_common_names(language_id = language_id, page = 0, per = 10)$data
  })
  expect_true(all(x$language_id == language_id))
})

test_that("tw_common_names_name", {
  vcr::use_cassette("tw_common_names_name", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    name <- tw_common_names(page = 5, per = 1)$data$name[1]
    x <- tw_common_names(name = name, page = 0, per = 10)$data
  })
  expect_true(x$name == name)
})

test_that("tw_common_names_otu_id", {
  vcr::use_cassette("tw_common_names_otu_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    otu_id <- tw_common_names(page = 5, per = 3)$data$otu_id
    x <- tw_common_names(otu_id = otu_id, page = 0, per = 10)$data
  })
  expect_true(all(x$otu_id %in% otu_id))
  expect_true(nrow(x) >= 3)
})