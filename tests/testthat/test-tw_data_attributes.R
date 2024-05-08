skip_on_cran()

test_that("tw_data_attributes", {
  vcr::use_cassette("tw_data_attributes", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_data_attributes(page = 0, per = 1)$data
  })

  expect_true("predicate_name" %in% names(x))
  expect_true("value" %in% names(x))
  expect_equal(nrow(x), 1)
})

test_that("tw_data_attibutes_attribute_subject_id", {
  vcr::use_cassette("tw_data_attibutes_attribute_subject_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    attribute_subject_id <- tw_data_attributes(page = 0, per = 1)$data$attribute_subject_id[1]
    x <- tw_data_attributes(attribute_subject_id = attribute_subject_id, page = 0, per = 10)$data
  })
  expect_true(all(x$attribute_subject_id == attribute_subject_id))
})

test_that("tw_data_attributes_attribute_subject_type", {
  vcr::use_cassette("tw_data_attributes_attribute_subject_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    attribute_subject_type <- tw_data_attributes(page = 0, per = 1)$data$attribute_subject_type[1]
    x <- tw_data_attributes(attribute_subject_type = attribute_subject_type, page = 0, per = 10)$data
  })
  expect_true(all(x$attribute_subject_type == attribute_subject_type))
})

test_that("tw_data_attributes_controlled_vocabulary_term_id", {
  vcr::use_cassette("tw_data_attributes_controlled_vocabulary_term_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    controlled_vocabulary_term_id <- tw_controlled_vocabulary_terms(page = 0, per = 10)$data$id
    x <- tw_data_attributes(controlled_vocabulary_term_id = controlled_vocabulary_term_id, page = 0, per = 10)$data
  })
  expect_true(all(x$controlled_vocabulary_term_id %in% controlled_vocabulary_term_id))
})

test_that("tw_data_attributes_data_attribute_id", {
  vcr::use_cassette("tw_data_attributes_data_attribute_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    data_attribute_id <- tw_data_attributes(page = 5, per = 1)$data$id[1]
    x <- tw_data_attributes(data_attribute_id = data_attribute_id, page = 0, per = 10)$data
  })
  expect_true(all(x$id == data_attribute_id))
})

test_that("tw_data_attributes_type", {
  vcr::use_cassette("tw_data_attributes_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    type <- tw_data_attributes(page = 0, per = 1)$data$type[1]
    x <- tw_data_attributes(type = type, page = 0, per = 10)$data
  })
  expect_true(all(x$type == type))
})

test_that("tw_data_attributes_import_predicate", {
  vcr::use_cassette("tw_data_attributes_import_predicate", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    import_predicate <- tw_data_attributes(page = 0, per = 10)$data$import_predicate[1]
    import_predicate <- import_predicate[!is.na(import_predicate)][1]
    x <- tw_data_attributes(import_predicate = import_predicate, page = 0, per = 10)$data
  })
  expect_true(all(x$import_predicate == import_predicate))
})

test_that("tw_data_attributes_value", {
  vcr::use_cassette("tw_data_attributes_value", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    value <- tw_data_attributes(page = 5, per = 1)$data$value[1]
    x <- tw_data_attributes(value = value, page = 0, per = 10)$data
  })
  expect_true(all(x$value == value))
})