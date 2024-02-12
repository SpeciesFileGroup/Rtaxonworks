skip_on_cran()

test_that("tw_projects", {
  vcr::use_cassette("tw_projects", {
    x <- tw_projects()
  })

  expect_true(x$meta$success)
})
