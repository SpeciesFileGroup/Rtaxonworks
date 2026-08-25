#' Cached maps
#'
#' A CachedMap is a pre-computed geographic distribution map for an OTU, with
#' synchronisation metadata and source-scope statistics. Only single-record
#' fetches are supported.
#'
#' @export
#' @template args
#' @param id (integer) the CachedMap id to fetch (required)
#' @return list
#' @examples
#' \dontrun{
#' tw_cached_maps(id = 1)
#' }
tw_cached_maps <- function(id, csv = FALSE, token = NULL, project_token = NULL,
                           page = 0, per = 50, ...) {

  assert(id, c("numeric", "integer"))
  if (missing(id) || is.null(id)) stop("`id` is required for tw_cached_maps()", call. = FALSE)

  args <- cc(list(token = token, project_token = project_token))

  res <- tw_GET(api_base_url(), sprintf("/cached_maps/%d", id),
                query = args, csv = csv, ...)
  return(res)
}

#' Cached maps
#' @rdname tw_cached_maps
#' @export
tw_cm <- tw_cached_maps
