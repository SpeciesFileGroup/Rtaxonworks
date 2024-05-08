skip_on_cran()

test_that("taxon_name_classifications", {
  vcr::use_cassette("taxon_name_classifications", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_name_classifications(page = 0, per = 1)$data
  })

  expect_equal(strsplit(x$type[1], "::")[[1]][1], 'TaxonNameClassification')
})

test_that("taxon_name_classifications_taxon_name_id", {
  vcr::use_cassette("taxon_name_classifications_taxon_name_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    taxon_name_id <- tw_taxon_name_classifications(page = 100, per = 1)$data$taxon_name_id[1]
    x <- tw_taxon_name_classifications(taxon_name_id = taxon_name_id, page = 0, per = 1)$data
  })
  expect_equal(x$taxon_name_id[1], taxon_name_id)
})

test_that("taxon_name_classifications_taxon_classification_type", {
  vcr::use_cassette("taxon_name_classifications_taxon_classification_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    taxon_name_id <- tw_taxon_name_classifications(page = 100, per = 1)$data$taxon_name_id[1]
    x <- tw_taxon_name_classifications(taxon_name_classification_type = "TaxonNameClassification::Latinized::Gender::Feminine", page = 0, per = 10)$data
  })
  expect_true(all(x$type == "TaxonNameClassification::Latinized::Gender::Feminine"))
})

test_that("taxon_name_classifications_taxon_classification_set", {
  vcr::use_cassette("taxon_name_classifications_taxon_classification_set", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    taxon_name_id <- tw_taxon_name_classifications(page = 100, per = 1)$data$taxon_name_id[1]
    x <- tw_taxon_name_classifications(taxon_name_classification_set = "validating", page = 0, per = 10)$data
  })
  expect_true(all(grepl("Valid", x$type)))
})