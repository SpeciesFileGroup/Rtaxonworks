skip_on_cran()

test_that("tw_biological_associations", {
  vcr::use_cassette("tw_biological_associations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations()$data
  })
  expect_equal(x$base_class[1], 'BiologicalAssociation')
})

test_that("tw_biological_associations_array_params", {
  vcr::use_cassette("tw_biological_associations_array_params", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    one <- nrow(tw_biological_associations(otu_id = c(112685))$data)
    two <- nrow(tw_biological_associations(otu_id = c(112685, 112699))$data)
  })
  expect_true(one < two)
})

test_that("tw_biological_associations_simple", {
  vcr::use_cassette("tw_biological_associations_simple", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    res <- tw_biological_associations(subresource = "simple")$data
  })
  expect_true("object_order" %in% colnames(res))
})
