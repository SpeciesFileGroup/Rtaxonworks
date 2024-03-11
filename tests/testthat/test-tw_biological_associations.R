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

test_that("tw_biological_associations_biological_association_id", {
  vcr::use_cassette("tw_biological_associations_biological_association_id", {
    ids <- tw_biological_associations(per = 5)$data$id

    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    res <- tw_biological_associations(biological_association_id = ids)$data
  })
  expect_true(all(ids %in% res$id))
})

test_that("tw_biological_associations_biological_relationship_id", {
  vcr::use_cassette("tw_biological_associations_biological_relationship_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    rel_id <- tw_biological_associations(per = 1)$data$biological_relationship_id
    res <- tw_biological_associations(biological_relationship_id = rel_id)$data
  })
  expect_true(all(rel_id == res$biological_relationship_id))
})

# TODO: devise a test for collecting_event_id and collection_object_id that don't depend on specific data


# test_that("tw_biological_associations_descendants", {
#   vcr::use_cassette("tw_biological_associations_descendants", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     tn_id = tw_tn(name = "Singapora", name_exact = TRUE, rank="genus")$data$id[1]
#     res <- tw_biological_associations(taxon_name_id = tn_id, descendants = TRUE)$data
#   })
#   
# })