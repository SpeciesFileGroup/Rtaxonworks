skip_on_cran()

assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)

all_observation_objects <- tw_observations(page = 0, per = 1)$meta$total

test_that("tw_observations", {
  vcr::use_cassette("tw_observations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(page = 0, per = 1)$data
  })

  expect_true("observation_object_id" %in% colnames(x))
  expect_equal(nrow(x), 1)
})

test_that("tw_observations_character_state_id", {
  vcr::use_cassette("tw_observations_character_state_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    character_state_id <- tw_observations(page = 0, per = 10)$data$character_state_id
    character_state_id <- character_state_id[!is.na(character_state_id)]
    x <- tw_observations(character_state_id = character_state_id, page = 0, per = 10)$data
  })
  expect_true(all(x$character_state_id %in% character_state_id))
})

test_that("tw_observations_descriptor_id", {
  vcr::use_cassette("tw_observations_descriptor_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    descriptor_id <- tw_observations(page = 5, per = 1)$data$descriptor_id
    descriptor_id <- descriptor_id[!is.na(descriptor_id)]
    x <- tw_observations(descriptor_id = descriptor_id, page = 0, per = 10)$data
  })
  expect_true(all(x$descriptor_id == descriptor_id))
})

test_that("tw_observations_observation_id", {
  vcr::use_cassette("tw_observations_observation_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    observation_id <- tw_observations(page = 0, per = 3)$data$id
    observation_id <- observation_id[!is.na(observation_id)]
    x <- tw_observations(observation_id = observation_id, page = 0, per = 10)$data
  })
  expect_true(all(x$id %in% observation_id))
})

# TODO: Figure out how to test collection_object_id
# test_that("tw_observations_collection_object_id", {
#   vcr::use_cassette("tw_observations_collection_object_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     collection_object_id <- tw_observations(page = 0, per = 10)$data$observation_object_id
#     collection_object_id <- collection_object_id[!is.na(collection_object_id)]
#     x <- tw_observations(collection_object_id = collection_object_id, page = 0, per = 10)$data
#   })
#   expect_true(all(x$observation_object_id %in% collection_object_id))
# })


# TODO: Test observation_matrix_id and observation_object_global_id

test_that("tw_observations_observation_object_type", {
  vcr::use_cassette("tw_observations_observation_object_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    observation_object_type <- tw_observations(page = 0, per = 1)$data$observation_object_type[1]
    x <- tw_observations(observation_object_type = observation_object_type, page = 0, per = 10)$data
  })
  expect_true(all(x$observation_object_type == observation_object_type))
})


test_that("tw_observations_observation_type", {
  vcr::use_cassette("tw_observations_observation_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    observation_type <- tw_observations(page = 0, per = 1)$data$type[1]
    x <- tw_observations(observation_type = observation_type, page = 0, per = 10)$data
  })
  expect_true(all(x$type %in% observation_type))
})

test_that("tw_observations_otu_id", {
  vcr::use_cassette("tw_observations_otu_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    otu_id <- tw_observations(observation_object_type = "Otu", page = 0, per = 10)$data$observation_object_id[1]
    x <- tw_observations(otu_id = otu_id, page = 0, per = 10)$data
  })
  expect_true(all(x$observation_object_id == otu_id))
})

test_that("tw_observations_taxon_name_id", {
  vcr::use_cassette("tw_observations_taxon_name_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    otu_id <- tw_observations(observation_object_type = "Otu", page = 5, per = 1)$data$observation_object_id[1]
    taxon_name_id <- tw_otus(otu_id = otu_id, page = 0, per = 1)$data$taxon_name_id[1]
    x <- tw_observations(taxon_name_id = taxon_name_id, observation_object_type = "Otu", page = 0, per = 10)$data
  })
  expect_true(all(otu_id == x$observation_object_id))
  expect_true(all("Otu" == x$observation_object_type))
})

test_that("tw_observations_citations", {
  vcr::use_cassette("tw_observations_citations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(citations = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_citation_documents", {
  vcr::use_cassette("tw_observations_citation_documents", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(citation_documents = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_data_depictions", {
  vcr::use_cassette("tw_observations_data_depictions", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(data_depictions = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_depictions", {
  vcr::use_cassette("tw_observations_depictions", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(depictions = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_identifiers", {
  vcr::use_cassette("tw_observations_identifiers", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(identifiers = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_notes", {
  vcr::use_cassette("tw_observations_notes", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(notes = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_note_text", {
  vcr::use_cassette("tw_observations_note_text", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(note_text = "the", page = 0, per = 1)
    if (x$meta$total > 0) {
      note_text <- tw_notes(note_object_id = x$data$id, note_object_type = "Observation")$data$text
      expect_true("the" %in% note_text)
    } else {
      expect_true(x$meta$total < all_observation_objects)
    }
  })
})

test_that("tw_observations_protocols", {
  vcr::use_cassette("tw_observations_protocols", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(protocols = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})

test_that("tw_observations_tags", {
  vcr::use_cassette("tw_observations_tags", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_observations(tags = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_observation_objects)
})
