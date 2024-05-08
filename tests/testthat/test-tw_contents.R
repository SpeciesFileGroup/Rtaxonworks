skip_on_cran()

assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)

total_contents <- tw_contents(page = 0, per = 1)$meta$total

test_that("tw_contents", {
  vcr::use_cassette("tw_contents", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_contents(page = 0, per = 1)$data
  })

  expect_true("topic_id" %in% colnames(x))
  expect_equal(nrow(x), 1)
})

test_that("tw_contents_citations", {
  vcr::use_cassette("tw_contents_citations", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_contents(citations = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_citation_documents", {
  vcr::use_cassette("tw_contents_citation_documents", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_contents(citation_documents = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_origin_citation", {
  vcr::use_cassette("tw_contents_origin_citation", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_contents(origin_citation = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_images", {
  vcr::use_cassette("tw_contents_images", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_contents(images = TRUE, page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_image_id", {
  vcr::use_cassette("tw_contents_image_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    image_ids <- tw_images(depiction_object_type = "Content", page = 0, per = 1)$data$id
    x <- tw_contents(image_id = image_ids, page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_otu_id", {
  vcr::use_cassette("tw_contents_otu_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    otu_ids <- tw_otus(page = 0, per = 1)$data$id
    x <- tw_contents(otu_id = otu_ids, page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_text", {
  vcr::use_cassette("tw_contents_text", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    x <- tw_contents(text = "Test this is a foo.", page = 0, per = 1)
  })
  expect_true(x$meta$total < total_contents)
})

test_that("tw_contents_text_exact", {
  vcr::use_cassette("tw_contents_text_exact", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    non_exact <- tw_contents(text = "Test this is a foo.", page = 0, per = 1)
    exact <- tw_contents(text = "Test this is a foo.", exact = TRUE, page = 0, per = 1)
  })
  expect_true(exact$meta$total < non_exact$meta$total)
})

test_that("tw_contents_topic_id", {
  vcr::use_cassette("tw_contents_topic_id", {
    assign("TW_API_URL", "https://sandbox.taxonworks.org/api/v1", envir = .GlobalEnv)
    assign("TW_PROJECT_TOKEN", Sys.getenv("TW_PROJECT_TOKEN"), envir = .GlobalEnv)
    assign("TW_USER_TOKEN", Sys.getenv("TW_USER_TOKEN"), envir = .GlobalEnv)
    topic_id <- tw_controlled_vocabulary_terms(type = "Topic", page = 0, per = 10)$data$id
    x <- tw_contents(topic_id = topic_id, page = 0, per = 10)
  })
  expect_true(all(x$data$topic_id %in% topic_id))
})