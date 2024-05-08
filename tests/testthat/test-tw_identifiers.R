skip_on_cran()

test_that("tw_identifiers_identifier", {
  vcr::use_cassette("tw_identifiers_identifier", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)

    identifier <- tw_identifiers(identifier_object_type = "Source")$data$identifier[5]
    x <- tw_identifiers(identifier = identifier, identifier_object_type = "Source")$data
  })
  expect_true(grepl(identifier, x$identifier[1]))
  expect_equal(nrow(x), 1)
})

test_that("tw_identifiers_identifier_type", {
  vcr::use_cassette("tw_identifiers_identifier_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)

    identifiers <- tw_identifiers(identifier_object_type = "Source")$data
  })
  expect_true(all(identifiers$identifier_object_type == "Source"))
})
