skip_on_cran()

test_that("tw_up", {
  vcr::use_cassette("tw_up", {
    x <- tw_up()
  })

  expect_true(x)
})

