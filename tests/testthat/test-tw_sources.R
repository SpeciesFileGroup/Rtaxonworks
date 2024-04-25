skip_on_cran()

all_sources <- tw_sources(page = 0, per = 1)$meta$total

test_that("tw_sources_author", {
  vcr::use_cassette("tw_sources_author", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(author = "Smith", page = 0, per = 1)$data
  })
  expect_true(grepl("Smith", x$author[1]))
  expect_equal(nrow(x), 1)
})

test_that("tw_sources_author_exact", {
  vcr::use_cassette("tw_sources_author_exact", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(author = "Smith", exact_author = TRUE, page = 0, per = 1)$data
  })
  expect_equal(x$author[1], "Smith")
  expect_equal(nrow(x), 1)
})

test_that("tw_sources_author_id", {
  vcr::use_cassette("tw_sources_author_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    person <- tw_people(first_name = "E. L.", last_name = "Mockford", role = "SourceAuthor", page = 0, per = 10)$data
    x <- tw_sources(author = "Smith", author_id = person$id, author_id_or = TRUE, page = 0, per = 1)$data
  })
  expect_true(grepl("Mockford, E. L.", x$author[1]))
  expect_equal(nrow(x), 1)
})

test_that("tw_bibtex_type", {
  vcr::use_cassette("tw_bibtex_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(bibtex_type = "proceedings", page = 0, per = 10)$data
  })
  expect_true(all(x$bibtex_type == "proceedings"))
})

test_that("tw_sources_in_project", {
  vcr::use_cassette("tw_sources_in_project", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    in_project <- tw_sources(in_project = TRUE, page = 0, per = 1)$meta$total
  })
  expect_true(in_project < all_sources)
})

test_that("tw_sources_citations", {
  vcr::use_cassette("tw_sources_citations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    sources <- tw_sources(citations = TRUE, citation_object_type = "TaxonName", in_project = TRUE, page = 0, per = 1)
    x_citations <- tw_citations(citation_object_type = "TaxonName", source_id = sources$data$id[1], page = 0, per = 10)
  })
  expect_true(!is.na(sources$data$id[1]))
  expect_true(all(x_citations$source_id == sources$data$id[1]))
})

# TODO: test citations_on_otus parameter (possibly no otu citation data in 3i on sandbox)
#       not sure if it works?
#       https://sandbox.taxonworks.org/tasks/sources/filter?per=50&taxon_name_id%5B%5D=249932&extend%5B%5D=documents&page=1&citations_on_otus=false
#       https://sandbox.taxonworks.org/tasks/sources/filter?per=50&taxon_name_id%5B%5D=249932&extend%5B%5D=documents&page=1&citations_on_otus=true

# TODO: there is a descendants parameter in the API docs but it got removed or doesn't work?

test_that("tw_sources_documents", {
  vcr::use_cassette("tw_sources_documents", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", NULL, envir = .GlobalEnv)
    assign("TW_USER_TOKEN", NULL, envir = .GlobalEnv)
    sources <- tw_sources(documents = TRUE, in_project = FALSE, extend=c("documents"), page = 0, per = 1)$meta$total
  })
  expect_true(sources < all_sources)
})

# Note: journal will not actually be empty if there is a serial_id set so both journal and serial_id need to be empty to really get an empty journal field
#       serial_id is missing in the frontend so it's not (yet) possible to query on actually empty journal values in the frontend
test_that("tw_sources_empty", {
  vcr::use_cassette("tw_sources_empty", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    source <- tw_sources(empty = c("journal", "serial_id"), page = 0, per = 10)$data
  })
  expect_true(all(is.na(source$journal)))
})

# TODO: API bug? This does return sources with empty DOIs: https://sandbox.taxonworks.org/api/v1/sources?not_empty[]=doi&page=0&per=10
#       Is it coming from global DOI identifiers and not being added to the json?
#       Two bad records? https://sandbox.taxonworks.org/api/v1/sources?not_empty[]=doi&global_identifiers=false
#       This seems to do what it's supposed to do: https://sandbox.taxonworks.org/api/v1/sources?not_empty[]=doi&global_identifiers=true&per=100
# test_that("tw_sources_not_empty", {
#   vcr::use_cassette("tw_sources_not_empty", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     source <- tw_sources(not_empty = c("doi", "doi"), page = 0, per = 10)$data
#   })
#   expect_true(all(!is.na(source$doi)))
# })

test_that("tw_sources_global_identifier", {
  vcr::use_cassette("tw_sources_global_identifier", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(global_identifiers = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_local_identifier", {
  vcr::use_cassette("tw_sources_local_identifier", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(local_identifiers = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_match_identifiers", {
  vcr::use_cassette("tw_sources_match_identifiers", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    dois <- tw_sources(with_doi = TRUE, page = 10, per = 3)$data$doi
    tab_delimited_dois <- paste(dois, collapse = "\t")
    x <- tw_sources(match_identifiers = tab_delimited_dois, match_identifiers_type = "identifier", match_identifiers_delimiter = "\t", page = 0, per = 10)$data
  })
  expect_true(all(x$doi %in% dois))
  expect_equal(nrow(x), 3)
})

test_that("tw_sources_identifier", {
  vcr::use_cassette("tw_sources_identifier", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    doi <- tw_sources(not_empty = c("doi", "doi"), per = 1)$data$doi
    x <- tw_sources(identifier = doi, identifier_object_type = "Source", identifier_exact = TRUE)$data
  })
  expect_true(grepl(doi, x$doi[1]))
  expect_equal(nrow(x), 1)
})

# Note: identifier_start and identifier_end work but you will not be able to see the identifiers in the response and they might belong to another project unless in_project=true
#   Here just testing to see that you get fewer results than all_sources with the range parameters
test_that("tw_sources_identifiers_range", {
  vcr::use_cassette("tw_sources_identifiers_range", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(identifier_start = 1, identifier_end = 100, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_nomenclature", {
  vcr::use_cassette("tw_sources_nomenclature", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(nomenclature = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_namespace_id", {
  vcr::use_cassette("tw_sources_namespace_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(namespace_id = 137, page = 0, per = 10)
    })
  expect_true(all(x$namespace_id == 137))
})

# TODO: notes parameter is not working in the API?  https://github.com/SpeciesFileGroup/taxonworks/issues/3927
# test_that("tw_sources_notes", {
#   vcr::use_cassette("tw_sources_notes", {
#     assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
#     assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
#     assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
#     x <- tw_sources(notes = TRUE, page = 0, per = 1)
#   })
#   expect_true(x$meta$total < all_sources)
# })

test_that("tw_sources_roles", {
  vcr::use_cassette("tw_sources_roles", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(roles = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_serial", {
  vcr::use_cassette("tw_sources_serial", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(serial = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_serial_id", {
  vcr::use_cassette("tw_sources_serial_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    serial_id <- tw_sources(serial = TRUE, page = 0, per = 1)$data$serial_id
    x <- tw_sources(serial_id = c(serial_id, serial_id), page = 0, per = 10)$data
  })
  expect_true(all(x$serial_id == serial_id))
  expect_true(all(!is.na(x$serial_id)))
})

test_that("tw_sources_source_type", {
  vcr::use_cassette("tw_sources_source_type", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(source_type = "Source::Verbatim", page = 0, per = 10)$data
  })
  expect_true(all(x$type == "Source::Verbatim"))
})

test_that("tw_sources_tags", {
  vcr::use_cassette("tw_sources_tags", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(tags = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_topic_id", {
  vcr::use_cassette("tw_sources_topic_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(topic_id = c(623, 628), page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_title", {
  vcr::use_cassette("tw_sources_title", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(title = "A new species of", page = 0, per = 10)$data
  })
  expect_true(all(grepl("A|a new species of", x$title)))
})

test_that("tw_sources_exact_title", {
  vcr::use_cassette("tw_sources_exact_title", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(title = "A new species of", exact_title = TRUE, page = 0, per = 10)$data
  })
  expect_true(all(grepl("A new species of", x$title)))
})

test_that("tw_sources_query_term", {
  vcr::use_cassette("tw_sources_query_term", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(query_term = "new genus", page = 0, per = 10)$data
  })
  expect_true(all(grepl("genus", x$cached)))
  expect_true(all(grepl("new", x$cached)))
})

test_that("tw_sources_user_id", {
  vcr::use_cassette("tw_sources_user_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    user_id <- tw_sources(page = 101, per = 1)$data$created_by_id
    x <- tw_sources(user_id = user_id, user_target = "created", page = 0, per = 10)$data
  })
  expect_true(all(x$created_by_id == user_id))
})

test_that("tw_sources_user_date", {
  vcr::use_cassette("tw_sources_user_date", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(user_date_start = "2024-01-01", user_date_end="2024-01-30", user_target = "created", page = 0, per = 10)$data
  })
  expect_true(all(x$created_at >= "2024-01-01" & x$created_at <= "2024-01-30"))
})

test_that("tw_sources_with_doi", {
  vcr::use_cassette("tw_sources_with_doi", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(with_doi = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_with_title", {
  vcr::use_cassette("tw_sources_with_title", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(with_title = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < all_sources)
})

test_that("tw_sources_year_range", {
  vcr::use_cassette("tw_sources_year_range", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(year_start = 2000, year_end = 2001, page = 0, per = 10)$data
  })
  expect_true(all(x$year >= 2000 & x$year <= 2001))
})

test_that("tw_sources_wildcard_attribute_journal", {
  vcr::use_cassette("tw_sources_wildcard_attribute_journal", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT", Sys.getenv("TW_PROJECT"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_sources(journal = "Journal of", wildcard_attribute = c("journal", "journal"), page = 0, per = 10)$data
  })
  expect_true(all(grepl("Journal of", x$journal)))
})


# TODO: add tests for venn and venn_mode parameters