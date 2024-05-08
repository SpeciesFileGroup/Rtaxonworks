skip_on_cran()

all_taxon_names <- tw_taxon_names(page = 0, per = 1)$meta$total

test_that("tw_taxon_names_autocomplete", {
  vcr::use_cassette("tw_taxon_names_autocomplete", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names_autocomplete(term = "Haplotaxi")$data
  })
  expect_equal(x$label[1], "Haplotaxida")
})

# TODO: devise a better test for exact matching
test_that("tw_taxon_names_autocomplete_exact", {
  vcr::use_cassette("tw_taxon_names_autocomplete_exact", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names_autocomplete(term = "Haplotaxida", exact = TRUE)$data
  })
  expect_equal(x$label[1], "Haplotaxida")
})


# TODO: no_leaves option doesn't work in API?

# test_that("tw_taxon_names_autocomplete_with_leaves", {
#   vcr::use_cassette("tw_taxon_names_autocomplete_with_leaves", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     suggestions <- tw_taxon_names_autocomplete(term = "A", no_leaves = FALSE)$data
#     x <- tw_taxon_names(parent_id = suggestions$id[1], page = 0, per = 10)
#   })
#   expect_true(x$meta$total > 0)
# })

# test_that("tw_taxon_names_autocomplete_no_leaves", {
#   vcr::use_cassette("tw_taxon_names_autocomplete_no_leaves", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     suggestions <- tw_taxon_names_autocomplete(term = "A", no_leaves = TRUE)$data
#     x <- tw_taxon_names(parent_id = suggestions$id[1], page = 0, per = 10)
#   })
#   expect_true(x$meta$total == 0)
# })

test_that("tw_taxon_names_autocomplete_nomenclature_group", {
  vcr::use_cassette("tw_taxon_names_autocomplete_nomenclature_group", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    taxon_name_id <- tw_taxon_names_autocomplete(term = "Haplo", nomenclature_group = c("Species", "Species"))$data$id
    x <- tw_taxon_names(taxon_name_id = taxon_name_id, page = 0, per = 1)$data
  })
  expect_true(grepl("::SpeciesGroup", x$rank_string[1]))
})

test_that("tw_taxon_names_autocomplete_type", {
  vcr::use_cassette("tw_taxon_names_autocomplete_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    suggested = tw_taxon_names_autocomplete(term = "Aed", type = "Combination")$data
    taxon_name_id <- suggested$id[1]
    taxon_name_label <- suggested$label[1]
    x <- tw_taxon_names(taxon_name_id = taxon_name_id, page = 0, per = 1)$data
  })
  expect_equal("Combination", x$type[1])
  expect_equal(taxon_name_label, x$name_string[1])
})

test_that("tw_taxon_names_autocomplete_parent_id", {
  vcr::use_cassette("tw_taxon_names_autocomplete_parent_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tn1 <- tw_taxon_names(rank = "species", page = 3, per = 1)$data
    tn1_name <- tn1$cached[1]
    parent_id <- tn1$parent_id[1] - 1
    with_x <- tw_taxon_names_autocomplete(term = tn1_name, parent_id = parent_id, page = 0, per = 10)
    without_x <- tw_taxon_names_autocomplete(term = tn1_name, page = 0, per = 10)
  })
  expect_true(length(with_x$data) < length(without_x$data))  # expect to get no results with wrong parent_id
})

test_that("tw_taxon_names_autocomplete_valid", {
  vcr::use_cassette("tw_taxon_names_autocomplete_valid", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names_autocomplete(term = "A", valid = TRUE, page = 0, per = 10)$data
  })
  expect_true(all(x$id == x$valid_taxon_name_id))
})


test_that("tw_taxon_names_autocomplete_invalid", {
  vcr::use_cassette("tw_taxon_names_autocomplete_invalid", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names_autocomplete(term = "A", valid = FALSE, page = 0, per = 10)$data
  })
  expect_true(all(x$id != x$valid_taxon_name_id))
})



test_that("tw_taxon_names", {
  vcr::use_cassette("tw_taxon_names", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(page = 0, per = 1)$data
  })
  expect_equal(x$name[1], "Root")
})


test_that("tw_taxon_names_ancestors", {
  vcr::use_cassette("tw_taxon_names_ancestors", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    animalia_id <- tw_taxon_names(name = "Animalia")$data$id[1]
    x <- tw_taxon_names(taxon_name_id = animalia_id, ancestors = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total > 1)
  expect_equal(x$data$name[1], "Root")
})

# descendants = true: returns only the decendents
# descendants = false: returns the taxon name with taxon_name_id and its ancestors  (TODO: Is that the correct behavior? API docs wrong?)
# descendants absent: returns the taxon name with taxon_name_id
test_that("tw_taxon_names_descendants", {
  vcr::use_cassette("tw_taxon_names_descendants", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    animalia_id <- tw_taxon_names(name = "Animalia")$data$id[1]
    x <- tw_taxon_names(taxon_name_id = animalia_id, descendants = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total > 1)
  expect_true(x$data$name[1] != "Animalia")
})

test_that("tw_taxon_names_descendants_max_depth", {
  vcr::use_cassette("tw_taxon_names_descendants_max_depth", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    animalia_id <- tw_taxon_names(name = "Animalia")$data$id[1]
    no_depth <- tw_taxon_names(taxon_name_id = animalia_id, descendants = TRUE, page = 0, per = 1)
    depth1 <- tw_taxon_names(taxon_name_id = animalia_id, descendants = TRUE, descendants_max_depth = 1, page = 0, per = 1)
    depth2 <- tw_taxon_names(taxon_name_id = animalia_id, descendants = TRUE, descendants_max_depth = 2, page = 0, per = 1)
  })
  expect_true(depth1$meta$total < depth2$meta$total)
  expect_true(depth1$meta$total < no_depth$meta$total)
  expect_true(depth1$data$name[1] != "Animalia")
})

test_that("tw_taxon_names_descendants_false", {
  vcr::use_cassette("tw_taxon_names_descendants_false", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    animalia_id <- tw_taxon_names(name = "Animalia")$data$id[1]
    x <- tw_taxon_names(taxon_name_id = animalia_id, descendants = FALSE, page = 0, per = 1)
  })
  expect_true(x$meta$total > 1)
  expect_true(x$data$name[1] == "Animalia")
})

test_that("tw_taxon_names_etymology", {
  vcr::use_cassette("tw_taxon_names_etymology", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(etymology = TRUE, page = 0, per = 1)
  })
  expect_true(all(!is.na(x$etymology)))
})

test_that("tw_taxon_names_etymology_false", {
  vcr::use_cassette("tw_taxon_names_etymology_false", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(etymology = FALSE, page = 0, per = 1)
  })
  expect_true(all(is.na(x$etymology)))
})

test_that("tw_taxon_names_author", {
  vcr::use_cassette("tw_taxon_names_author", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(author = "Smith", page = 0, per = 1)$data
  })
  expect_true(grepl("Smith", x$cached_author_year[1]))
})

test_that("tw_taxon_names_author_exact", {
  vcr::use_cassette("tw_taxon_names_author_exact", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(author = "Smith", exact_author = TRUE, page = 0, per = 10)$data
  })
  expect_true(all(x$verbatim_author[1] == "Smith"))
})

test_that("tw_taxon_names_authors", {
  vcr::use_cassette("tw_taxon_names_authors", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(authors = FALSE, page = 2, per = 10)$data
  })
  expect_true(all(is.na(x$cached_author_year)))
})

test_that("tw_taxon_names_citations", {
  vcr::use_cassette("tw_taxon_names_citations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(citations = TRUE, page = 2, per = 1)
    citation <- tw_citations(citation_object_id = x$data$id[1], citation_object_type = "TaxonName")
  })
  expect_true(citation$meta$total == 1)
})

test_that("tw_taxon_names_citation_documents", {
  vcr::use_cassette("tw_taxon_names_citation_documents", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(citation_documents = TRUE, page = 2, per = 1)
    if (x$meta$total == 0) {
      expect_true(x$meta$total == 0)
    } else {
      citation <- tw_citations(citation_object_id = x$data$id[1], citation_object_type = "TaxonName")
      sources <- tw_sources(documents = TRUE, match_identifiers = citation$data$source_id[1], match_identifiers_type = "internal", match_identifiers_delimiter = "\t")
      expect_true(sources$meta$total > 0)
    }
  })
})

test_that("tw_taxon_names_collecting_event_id", {
  vcr::use_cassette("tw_taxon_names_collecting_event_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    ce_id <- tw_collecting_events(page = 0, per = 1)$data$id[1]
    x <- tw_taxon_names(collecting_event_id = ce_id, page = 0, per = 10)
  })
  if (x$meta$total == 0) {
    expect_true(x$meta$total == 0)
  } else {
    expect_true(all(x$data$collecting_event_id == ce_id))
  }
})

test_that("tw_taxon_names_collection_object_id", {
  vcr::use_cassette("tw_taxon_names_collection_object_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    co_id <- tw_co(page = 0, per = 1)$data$id[1]
    x <- tw_taxon_names(collection_object_id = co_id, page = 0, per = 10)
  })
  if (x$meta$total == 0) {
    expect_true(x$meta$total == 0)
  } else {
    expect_true(all(x$data$collection_object_id == co_id))
  }
})

# TODO: what does combination_taxon_name_id do?
# test_that("tw_taxon_names_combination_taxon_name_id", {
#   vcr::use_cassette("tw_taxon_names_combination_taxon_name_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     combination_taxon_name_id <- tw_taxon_names(taxon_name_type = "Combination", page = 0, per = 1)$data$id[1]
#     x <- tw_taxon_names(combination_taxon_name_id = combination_taxon_name_id, page = 0, per = 1)
#   })
#   expect_true(x$meta$total < all_taxon_names)
# })

test_that("tw_taxon_names_data_attributes", {
  vcr::use_cassette("tw_taxon_names_data_attributes", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    taxon_names_with_attributes <- tw_taxon_names(data_attributes = TRUE, page = 0, per = 10)$meta$total
  })
  expect_true(taxon_names_with_attributes < all_taxon_names)
})

# test_that("tw_taxon_names_data_attributes_predicate_id", {
#   vcr::use_cassette("tw_taxon_names_data_attributes_predicate_id", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     predicate_id <- tw_data_attributes(attribute_subject_type = "TaxonName", page = 0, per = 1)$data$controlled_vocabulary_term_id[1]
#     x <- tw_taxon_names(data_attributes = TRUE, data_attribute_predicate_id = predicate_id, page = 0, per = 10)
#   })
#   if (x$meta$total == 0) {
#     expect_true(x$meta$total == 0)
#   } else {
#     expect_true(all(x$data$controlled_vocabulary_term_id == predicate_id))
#   }
# })

test_that("tw_taxon_names_name", {
  vcr::use_cassette("tw_taxon_names_name", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(name = "urpur", page = 0, per = 1)$data
  })
  expect_true(all(grepl("urpur", x$name_string)))
})

# TODO: Not sure if exact does anything?
test_that("tw_taxon_names_name_exact", {
  vcr::use_cassette("tw_taxon_names_name_exact", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(name = "urpur", exact = TRUE, page = 0, per = 1)$data
  })
  expect_true(all(grepl("urpur", x$name_string)))
})

test_that("tw_taxon_names_images", {
  vcr::use_cassette("tw_taxon_names_images", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    image_ids <- tw_images(page = 0, per = 3)$data$id
    x <- tw_taxon_names(image_id = image_ids, images = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_taxon_names)
  expect_true(x$meta$total > 0)
})

test_that("tw_taxon_names_image_id", {
  vcr::use_cassette("tw_taxon_names_image_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(image_id = 1, page = 0, per = 1)
  })
  expect_true(x$meta$total == 0)
})

test_that("tw_taxon_names_leaves", {
  vcr::use_cassette("tw_taxon_names_leaves", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tn_id <- tw_taxon_names(leaves = TRUE, page = 0, per = 1)$data$id[1]
    x <- tw_taxon_names(taxon_name_id = tn_id, descendants = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total == 0)
})

test_that("tw_taxon_names_note_text", {
  vcr::use_cassette("tw_taxon_names_note_text", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tn_id <- tw_taxon_names(note_text = "species-group", page = 0, per = 1)$data$id[1]
    note <- tw_notes(note_object_type = "TaxonName", note_object_id = tn_id, taxon_name = TRUE)$data$text[1]
  })
  expect_true(all(grepl("species-group", note)))
})

test_that("tw_taxon_names_notes", {
  vcr::use_cassette("tw_taxon_names_notes", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    tn_id <- tw_taxon_names(notes = TRUE, page = 0, per = 1)$data$id[1]
    x <- tw_notes(note_object_type = "TaxonName", note_object_id = tn_id, taxon_name = TRUE)
  })
  expect_true(x$meta$total > 0)
})

test_that("tw_taxon_names_nomenclature_code", {
  vcr::use_cassette("tw_taxon_names_nomenclature_code", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(nomenclature_code = "icn", page = 0, per = 1)$data
  })
  expect_equal(x$nomenclatural_code[1], "icn")
})

test_that("tw_taxon_names_nomenclature_group", {
  vcr::use_cassette("tw_taxon_names_nomenclature_group", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_taxon_names(nomenclature_group = "genus", page = 0, per = 1)$data
  })
  expect_true(all(grepl("GenusGroup", x$rank_string)))
})