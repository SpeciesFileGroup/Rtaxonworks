skip_on_cran()

assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)

all_observation_objects <- tw_observations(page = 0, per = 1)$meta$total

test_that("tw_observation_matrices", {
  vcr::use_cassette("tw_observation_matrices", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observation_matrices(page = 0, per = 1)
  })
  expect_equal(x$data$base_class[1], "ObservationMatrix")
  expect_equal(nrow(x$data), 1)
})

test_that("tw_observation_matrices_observation_matrix_id", {
  vcr::use_cassette("tw_observation_matrices_observation_matrix_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    observation_matrix_id <- tw_observation_matrices(page = 0, per = 2)$data$id
    x <- tw_observation_matrices(observation_matrix_id = observation_matrix_id, page = 0, per = 2)
  })

  expect_true(all(x$data$id %in% observation_matrix_id))
})
