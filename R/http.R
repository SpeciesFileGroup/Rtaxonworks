api_base_url <- function() {
  if (!exists("TW_API_URL", envir = globalenv())) {
    return("https://sfg.taxonworks.org/api/v1")
  }
  get("TW_API_URL", envir = globalenv())
}

api_user_token <- function() {
  if (!exists("TW_USER_TOKEN", envir = globalenv())) {
    return()
  }
  get("TW_USER_TOKEN", envir = globalenv())
}

api_project_token <- function() {
  if (!exists("TW_PROJECT_TOKEN", envir = globalenv())) {
    cat("\033[38;2;255;255;0mNo TW_PROJECT_TOKEN variable is set\033[0m\n")
    return()
  }
  get("TW_PROJECT_TOKEN", envir = globalenv())
}

add_token_params <- function(query) {
  query <- c(query, list(
                         token = api_user_token(),
                         project_token = api_project_token()))
  return(cc(query))
}

serialize <- function(params, vector_params) {
  url_params <- c()
  for (name in names(params)) {
    if (name %in% vector_params) {
      sep <- "[]="
    } else {
       sep <- "="
    }
    values <- as.vector(params[[name]])
    for (value in values) {
      if (is.numeric(value)) {
        value <- formatC(value, format = "f", digits = 0)  # eliminates scientific notation
      }
      url_params <- c(url_params,
                      paste0(name, sep, URLencode(as.character(value))))
    }
  }
  url_params_string <- paste(url_params, collapse = "&")
  url_params_string <- paste0("?", url_params_string)
  return(url_params_string)
}

tw_ua <- function(on_gh_actions = FALSE) {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
                paste0("httr2/", utils::packageVersion("httr2")),
                sprintf("SpeciesFileGroup(rtaxonworks/%s)",
                        utils::packageVersion("rtaxonworks")))
  if (on_gh_actions) versions <- c(versions, "GitHub Actions")
  paste0(versions, collapse = " ")
}
ongha <- as.logical(Sys.getenv("ON_GH_ACTIONS", FALSE))
tw_ual <- list(`User-Agent` = tw_ua(ongha), `X-USER-AGENT` = tw_ua(ongha))

#' Perform a GET request to the TaxonWorks API
#'
#' @importFrom httr2 request req_perform resp_body_string
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as.tibble tibble
#' @param url the base URL
#' @param path the endpoint path
#' @param query a list of query parameters
#' @param headers a list of headers
#' @keywords internal
#' @return a tibble of results
tw_GET <- function(
    url, path = NULL, query = list(), headers = list(), csv = FALSE, vector_params = c(), ...) {

  result <- list(meta = list(), data = tibble())

  if (csv) {
    path <- paste0(path, ".csv")
  }

  # if tokens are null then add tokens from global environment
  if (is.null(query$token) && is.null(query$project_token)) {
    query <- add_token_params(query)
  }


  url_params_string <- serialize(query, vector_params = vector_params)
  url <- paste0(api_base_url(), path, url_params_string)

  cat(sprintf("\033[32mGET %s\033[39m\n", url))

  if (csv) {
    return(as_tibble(read.csv(url, sep = "\t",
                              header = TRUE,
                              stringsAsFactors = FALSE,
                              na.strings = c("", "NA"))))
  } else {
    req <- request(url)
    resp <- req_perform(req)
    meta <- list(
      page = as.integer(resp$headers$`pagination-page`),
      next_page = as.integer(resp$headers$`pagination-next-page`),
      per = as.integer(resp$headers$`pagination-per-page`),
      total = as.integer(resp$headers$`pagination-total`),
      total_pages = as.integer(resp$headers$`pagination-total-pages`)
    )
    body <- resp_body_string(resp)

    if (body == "" || body == "[]" || body == "{}" ||
          is.null(body) || is.na(body)) {
      data <- tibble()
    } else {
      if (is.data.frame(fromJSON(body))) {
        data <- as_tibble(flatten(fromJSON(body), recursive = TRUE))
      } else {
        data <- as_tibble(fromJSON(body))
      }
    }
    result <- list(meta = meta, data = data)
    return(result)
  }
}
