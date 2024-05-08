skip_on_cran()

test_that("tw_tags", {
  vcr::use_cassette("tw_tags", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_tags(page = 0, per = 1)$data
  })

  expect_true("tag_object_id" %in% colnames(x))
  expect_equal(nrow(x), 1)
})

# TODO: Tag ID doesn't work in API?
# test_that("tw_tags_tag_id", {
#   vcr::use_cassette("tw_tags_tag_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     tag_id <- tw_tags(page = 5, per = 1)$data$id[1]
#     x <- tw_tags(tag_id = tag_id, page = 0, per = 10)$data
#   })
#   expect_true(all(x$id == tag_id))
# })

test_that("tw_tags_tag_object_id", {
  vcr::use_cassette("tw_tags_tag_object_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tag_object_id <- tw_tags(page = 5, per = 1)$data$tag_object_id[1]
    x <- tw_tags(tag_object_id = tag_object_id, page = 0, per = 10)$data
  })
  expect_true(all(x$tag_object_id == tag_object_id))
})

test_that("tw_tags_tag_object_type", {
  vcr::use_cassette("tw_tags_tag_object_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tag_object_type <- tw_tags(page = 5, per = 1)$data$tag_object_type[1]
    x <- tw_tags(tag_object_type = tag_object_type, page = 0, per = 10)$data
  })
  expect_true(all(x$tag_object_type == tag_object_type))
})
