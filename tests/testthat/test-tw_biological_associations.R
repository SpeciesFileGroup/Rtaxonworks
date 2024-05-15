skip_on_cran()

assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
all_ba_total <- tw_biological_associations(per = 1, csv = FALSE)$meta$total

test_that("tw_biological_associations", {
  vcr::use_cassette("tw_biological_associations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(per = 1, csv = FALSE)$data
  })
  expect_equal(x$base_class[1], 'BiologicalAssociation')
})

test_that("tw_biological_associations_array_params", {
  vcr::use_cassette("tw_biological_associations_array_params", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    ids = unique(tw_biological_associations(per = 10, csv = FALSE)$data$biological_association_subject_id)
    one <- nrow(tw_biological_associations(otu_id = ids[1], csv = FALSE)$data)
    two <- nrow(tw_biological_associations(otu_id = c(ids[1], ids[2]), csv = FALSE)$data)
  })
  expect_true(one < two)
})

test_that("tw_biological_associations_simple", {
  vcr::use_cassette("tw_biological_associations_simple", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    res <- tw_biological_associations(subresource = "simple", csv = FALSE)$data
  })
  expect_true("object_order" %in% colnames(res))
})

test_that("tw_biological_associations_biological_association_id", {
  vcr::use_cassette("tw_biological_associations_biological_association_id", {
    ids <- tw_biological_associations(per = 5, csv = FALSE)$data$id

    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    res <- tw_biological_associations(biological_association_id = ids, csv = FALSE)$data
  })
  expect_true(all(ids %in% res$id))
})

test_that("tw_biological_associations_biological_relationship_id", {
  vcr::use_cassette("tw_biological_associations_biological_relationship_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    rel_id <- tw_biological_associations(per = 1, csv = FALSE)$data$biological_relationship_id
    res <- tw_biological_associations(biological_relationship_id = rel_id, csv = FALSE)$data
  })
  expect_true(all(rel_id == res$biological_relationship_id))
})

# TODO: devise a test for collecting_event_id and collection_object_id that don't depend on specific data


test_that("tw_biological_associations_descendants", {
  vcr::use_cassette("tw_biological_associations_descendants", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tn_id = tw_tn(name = "Singapora", name_exact = TRUE, rank="genus")$data$id[1]
    res <- tw_biological_associations(taxon_name_id = tn_id, descendants = TRUE, csv = FALSE)$data
  })
  expect_true(nrow(res) > 0)
})

# TODO: What does exclude_taxon_name_relationship do?

test_that("tw_biological_associations_geo_json", {
  vcr::use_cassette("tw_biological_associations_geo_json", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    res <- tw_biological_associations(geo_json = '{"type":"MultiPolygon","coordinates":[[[[-34.365234,35.118711],[-34.365234,41.523933],[-22.851563,41.523933],[-22.851563,35.118711],[-34.365234,35.118711]]]]}', csv = FALSE)
  })
  expect_true(res$meta$total < all_ba_total)
})

test_that("tw_biological_associations_geographic_area_id", {
  vcr::use_cassette("tw_biological_associations_geographic_area_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    geographical_area_id <- tw_asserted_distributions(per = 1)$data$geographic_area_id[1]
    x <- tw_biological_associations(geographic_area_id = geographical_area_id, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

# TODO: What does geographic_area_mode do?

test_that("tw_biological_associations_object_biological_property_id", {
  vcr::use_cassette("tw_biological_associations_object_biological_property_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    object_biological_property_id <- tw_vocab(type = "BiologicalProperty", per = 1)$data$id[1]
    x <- tw_biological_associations(object_biological_property_id = TW_USER_TOKEN, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})


test_that("tw_biological_associations_wkt", {
  vcr::use_cassette("tw_biological_associations_wkt", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(wkt = "POLYGON((-34.365234 35.118711, -34.365234 41.523933, -22.851563 41.523933, -22.851563 35.118711, -34.365234 35.118711))", csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})


test_that("tw_biological_associations_citations", {
  vcr::use_cassette("tw_biological_associations_citations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    ba_id <- tw_biological_associations(citations = TRUE, csv = FALSE)$data$id[3]
    citation <- tw_citations(citation_object_type = "BiologicalAssociation", citation_object_id = ba_id)
  })
  #expect_true(x$meta$total < all_ba_total)  # TODO: Why does citations = true return more biological associations than the total number of biological associations?
  expect_equal(citation$data$citation_object_id[1], ba_id)
  expect_true(citation$meta$total > 0)
})

test_that("tw_biological_associations_citation_documents", {
  vcr::use_cassette("tw_biological_associations_citation_documents", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(citation_documents = TRUE, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_origin_citation", {
  vcr::use_cassette("tw_biological_associations_origin_citation", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(origin_citation = TRUE, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_identifiers", {
  vcr::use_cassette("tw_biological_associations_identifiers", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(identifiers = TRUE, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_local_identifiers", {
  vcr::use_cassette("tw_biological_associations_local_identifiers", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(local_identifiers = TRUE, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_notes", {
  vcr::use_cassette("tw_biological_associations_notes", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(notes = TRUE, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_keyword_id_and", {
  vcr::use_cassette("tw_biological_associations_keyword_id_and", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    keyword_ids <- tw_vocab(type = "Keyword")$data$id
    x <- tw_biological_associations(keyword_id_and = keyword_ids, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_keyword_id_or", {
  vcr::use_cassette("tw_biological_associations_keyword_id_or", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    keyword_ids <- tw_vocab(type = "Keyword")$data$id
    x <- tw_biological_associations(keyword_id_or = keyword_ids, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

test_that("tw_biological_associations_tags", {
  vcr::use_cassette("tw_biological_associations_tags", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_biological_associations(tags = TRUE, csv = FALSE)
  })
  expect_true(x$meta$total < all_ba_total)
})

# TODO: enable once the new endpoint is deployed on sandbox
# test_that("tw_biological_associations_extended", {
#   vcr::use_cassette("tw_biological_associations_extended", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     x <- tw_biological_associations(subresource = "extended", csv = FALSE)
#   })
#   expect_true("object_taxon_name_id" %in% colnames(res))
# })