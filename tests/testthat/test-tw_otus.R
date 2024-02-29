skip_on_cran()

test_that("tw_otus", {
  vcr::use_cassette("tw_otus", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_otus(page = 0, per = 1)
  })
  expect_equal(x$global_id[1], paste('gid://taxon-works/Otu/', as.character(x$id[1]), sep=''))
})
