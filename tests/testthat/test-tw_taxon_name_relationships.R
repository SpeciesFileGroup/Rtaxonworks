skip_on_cran()

test_that("tw_taxon_name_relationships", {
  vcr::use_cassette("tw_taxon_name_relationships", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_name_relationships(page = 0, per = 1)$data
  })
  expect_equal(strsplit(x$type[1], "::")[[1]][1], 'TaxonNameRelationship')
})

test_that("tw_taxon_name_relationships_object", {
  vcr::use_cassette("tw_taxon_name_relationships_object", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    object_taxon_name_id <- tw_taxon_name_relationships(page = 20, per = 1)$data$object_taxon_name_id[1]
    x <- tw_taxon_name_relationships(object_taxon_name_id = object_taxon_name_id, page = 0, per = 1)$data
  })
  expect_true(x$object_taxon_name_id[1] == object_taxon_name_id)
})

test_that("tw_taxon_name_relationships_subject", {
  vcr::use_cassette("tw_taxon_name_relationships_subject", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    subject_taxon_name_id <- tw_taxon_name_relationships(page = 20, per = 1)$data$subject_taxon_name_id[1]
    x <- tw_taxon_name_relationships(subject_taxon_name_id = subject_taxon_name_id, page = 0, per = 1)$data
  })
  expect_true(x$subject_taxon_name_id[1] == subject_taxon_name_id)
})

test_that("tw_taxon_name_relationships_taxon_name_id", {
  vcr::use_cassette("tw_taxon_name_relationships_taxon_name_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    taxon_name_id <- tw_taxon_name_relationships(page = 100, per = 1)$data$subject_taxon_name_id
    x <- tw_taxon_name_relationships(taxon_name_id = taxon_name_id, page = 0, per = 1)$data
  })
  expect_true(any(x$subject_taxon_name_id == taxon_name_id) || any(x$object_taxon_name_id == taxon_name_id))
})

test_that("tw_taxon_name_relationships_taxon_name_relationship_set", {
  vcr::use_cassette("tw_taxon_name_relationships_taxon_name_relationship_set", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_name_relationships(taxon_name_relationship_set = "synonym", page = 0, per = 10)$data
  })
  expect_true(all(grepl("Invalidating", x$type)))  # testing for Invalidating instead of Synonym because it still returns types like "TaxonNameRelationship::Iczn::Invalidating", "TaxonNameRelationship::Iczn::Invalidating::Usage::Misspelling"
})

test_that("tw_taxon_name_relationships_taxon_name_relationship_type", {
  vcr::use_cassette("tw_taxon_name_relationships_taxon_name_relationship_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_name_relationships(taxon_name_relationship_type = "TaxonNameRelationship::Iczn::Validating::UncertainPlacement", page = 0, per = 10)
  })
  expect_true(all(x$data$type == "TaxonNameRelationship::Iczn::Validating::UncertainPlacement"))
})
