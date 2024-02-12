# TODO: Get crul to handle TW arrays like: https://sandbox.taxonworks.org/api/v1/otus?page=0&per=50&token=&project_token=&otu_id[]=67407&otu_id[]=67408
#    setting crul opts = list(globoff = TRUE) may allow square brackets in the parameters

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

tw_ua <- function(on_gh_actions = FALSE) {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("SpeciesFileGroup(RTaxonWorks/%s)", utils::packageVersion("RTaxonWorks")))
  if (on_gh_actions) versions <- c(versions, "GitHub Actions")
  paste0(versions, collapse = " ")
}
ongha <- as.logical(Sys.getenv('ON_GH_ACTIONS', FALSE))
tw_ual <- list(`User-Agent` = tw_ua(ongha), `X-USER-AGENT` = tw_ua(ongha))

tw_GET <- function(url, path = NULL, query = list(), headers = list(),
  opts = list(), parse = TRUE, ...) {

  query = add_token_params(query)

  # if an array of IDs is passed, the query needs to have duplicate parameters like ?taxon_name_id[]=249667&taxon_name_id[]=249668

  cli <- crul::HttpClient$new(url,
    headers = c(headers, tw_ual), opts = c(opts, list(...)))
  out <- cli$get(path = path, query = query)
  return(tw_error_handle(out, parse = parse))
}

tw_POST <- function(url, path = NULL, query = list(), body = list(),
  headers = list(), opts = list(), parse = TRUE, ct = "text/json",
  ...) {

  
  
  cli <- crul::HttpClient$new(url,
    headers = c(headers, tw_ual, "Content-Type" = "text/json"),
    opts = c(opts, list(...)))
  out <- cli$post(path = path, query = query, body = body)
  return(tw_error_handle(out, parse = parse))
}

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
