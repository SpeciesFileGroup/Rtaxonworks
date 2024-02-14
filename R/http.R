api_base_url <- function() {
  if (!exists("TW_API_URL", envir = globalenv())) {
    assign("TW_API_URL", "https://sfg.taxonworks.org/api/v1", envir = globalenv())
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
    return()
  }
  get("TW_PROJECT_TOKEN", envir = globalenv())
}

add_token_params <- function(query) {
  query <- c(query, list(token = api_user_token(), project_token = api_project_token()))
  return(query)
}

serialize <- function(params) {
  url_params <- c()
  for (name in names(params)) {
    if (length(params[[name]]) > 1) {
      for (value in params[[name]]) {
        url_params <- c(url_params, paste0(name, "[]=", URLencode(as.character(value))))
      }
    } else {
      url_params <- c(url_params, paste0(name, "=", URLencode(as.character(params[[name]]))))
    }
  }
  url_params_string <- paste(url_params, collapse = "&")
  url_params_string <- paste0("?", url_params_string)
  return(url_params_string)
}

tw_ua <- function(on_gh_actions = FALSE) {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
    paste0("httr2/", utils::packageVersion("httr2")),
    sprintf("SpeciesFileGroup(RTaxonWorks/%s)", utils::packageVersion("RTaxonWorks")))
  if (on_gh_actions) versions <- c(versions, "GitHub Actions")
  paste0(versions, collapse = " ")
}
ongha <- as.logical(Sys.getenv('ON_GH_ACTIONS', FALSE))
tw_ual <- list(`User-Agent` = tw_ua(ongha), `X-USER-AGENT` = tw_ua(ongha))

#' Perform a GET request to the TaxonWorks API
#'
#' @importFrom httr2 request req_perform resp_body_json %>%
#' @param url the base URL
#' @param path the endpoint path
#' @param query a list of query parameters
#' @param headers a list of headers
#' @keywords internal
#' @return a list of JSON results
tw_GET <- function(url, path = NULL, query = list(), headers = list(), ...) {

  query <- add_token_params(query)
  url_params_string <- serialize(query)
  url <- paste0(api_base_url(), path, url_params_string)

  req <- request(url)
  resp <- req_perform(req)
  result <- resp %>% resp_body_json()
  return(result)
}  # TODO: need to add error handling


# tw_POST <- function(url, path = NULL, query = list(), body = list(),
#   headers = list(), opts = list(), parse = TRUE, ct = "text/json",
#   ...) {

#   cli <- crul::HttpClient$new(url,
#     headers = c(headers, tw_ual, "Content-Type" = "text/json"),
#     opts = c(opts, list(...)))
#   out <- cli$post(path = path, query = query, body = body)
#   return(tw_error_handle(out, parse = parse))
# }

tw_error_handle <- function(x, parse = TRUE) {
  if (x$status_code > 203) {
    txt <- tryCatch(x$parse("utf-8"), error = function(e) e)
    if (inherits(txt, "error")) x$raise_for_status()
    json <- tryCatch(jsonlite::fromJSON(txt), error = function(e) e)
    if (inherits(json, "error")) x$raise_for_status()
    stop(sprintf("%s %s: %s", x$status_http()$message, json$code, json$message),
      call. = FALSE)
  }
  txt <- x$parse("utf-8")
  jsonlite::fromJSON(txt, parse)
}
