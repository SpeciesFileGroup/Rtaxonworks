#' Leads (dichotomous keys)
#'
#' A Lead represents a node in a dichotomous identification key. The API
#' returns the full key structure as JSON, including metadata (title,
#' citation, attribution, taxonomic scope) and the couplet data.
#'
#' @export
#' @template args
#' @param id (integer) the root Lead id for the key (required)
#' @return list
#' @examples
#' \dontrun{
#' tw_leads(id = 1)
#' }
tw_leads <- function(id, csv = FALSE, token = NULL, project_token = NULL,
                     page = 0, per = 50, ...) {

  if (missing(id) || is.null(id)) stop("`id` is required for tw_leads()", call. = FALSE)
  assert(id, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token))

  res <- tw_GET(api_base_url(), sprintf("/leads/key/%d", id),
                query = args, csv = csv, ...)
  return(res)
}

#' Leads
#' @rdname tw_leads
#' @export
tw_ld <- tw_leads
