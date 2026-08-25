#' Deep health check
#'
#' Deep health check for the TaxonWorks instance (checks connectivity to
#' the database and other back-end services).
#'
#' @export
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_pingz()
#' }
tw_pingz <- function(csv = FALSE, token = NULL, project_token = NULL,
                     page = 0, per = 50, ...) {
  args <- cc(list(token = token, project_token = project_token))
  res <- tw_GET(api_base_url(), "/pingz", query = args, csv = csv, ...)
  return(res)
}


#' Project or global statistics
#'
#' Counts of records in the various TaxonWorks data models. Scoped to a project
#' when a `project_token` is provided (either as argument or via the
#' `TW_PROJECT_TOKEN` environment variable); otherwise global.
#'
#' @export
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_stats()
#' }
tw_stats <- function(csv = FALSE, token = NULL, project_token = NULL,
                     page = 0, per = 50, ...) {
  args <- cc(list(token = token, project_token = project_token))
  res <- tw_GET(api_base_url(), "/stats", query = args, csv = csv, ...)
  return(res)
}


#' Recent project activity
#'
#' Recent activity feed for the current project. Requires a project token.
#'
#' @export
#' @template args
#' @param past_days (integer) number of days of activity to include
#' @return list
#' @examples
#' \dontrun{
#' tw_activity(past_days = 7)
#' }
tw_activity <- function(past_days = NULL, csv = FALSE, token = NULL,
                        project_token = NULL, page = 0, per = 50, ...) {
  args <- cc(list(past_days = past_days,
                  token = token, project_token = project_token))
  res <- tw_GET(api_base_url(), "/activity", query = args, csv = csv, ...)
  return(res)
}


#' Count valid species for a taxon name
#'
#' Returns the number of valid species descending from the given taxon name.
#' Uses the unauthenticated `/cite/count_valid_species` endpoint.
#'
#' @export
#' @template args
#' @param taxon_name (string) exact taxon name string to search (required)
#' @return list
#' @examples
#' \dontrun{
#' tw_count_valid_species(taxon_name = "Aphrodinae")
#' }
tw_count_valid_species <- function(taxon_name, csv = FALSE, token = NULL,
                                   project_token = NULL, page = 0, per = 50, ...) {
  if (missing(taxon_name) || is.null(taxon_name))
    stop("`taxon_name` is required for tw_count_valid_species()", call. = FALSE)
  args <- cc(list(taxon_name = taxon_name,
                  token = token, project_token = project_token))
  res <- tw_GET(api_base_url(), "/cite/count_valid_species",
                query = args, csv = csv, ...)
  return(res)
}
