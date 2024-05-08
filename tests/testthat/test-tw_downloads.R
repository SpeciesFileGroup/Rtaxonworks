skip_on_cran()

# TODO: It seems like downloads might not be showing in the API?
# test_that("tw_downloads", {
#   vcr::use_cassette("tw_downloads", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     x <- tw_downloads(page = 2, per = 1)$data
#   })
#   expect_true("download_type" %in% names(x))
# })
