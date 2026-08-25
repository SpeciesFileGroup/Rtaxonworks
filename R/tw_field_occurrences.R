#' Field occurrences
#'
#' A FieldOccurrence records a biological entity observed in the field. The API
#' currently exposes a single endpoint returning Darwin Core occurrence
#' attributes for a given FieldOccurrence.
#'
#' @export
#' @template args
#' @param id (integer) the FieldOccurrence id (required)
#' @return list
#' @examples
#' \dontrun{
#' tw_field_occurrences(id = 1)
#' }
tw_field_occurrences <- function(id, csv = FALSE, token = NULL,
                                 project_token = NULL, page = 0, per = 50, ...) {

  if (missing(id) || is.null(id)) stop("`id` is required for tw_field_occurrences()", call. = FALSE)
  assert(id, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token))

  res <- tw_GET(api_base_url(), sprintf("/field_occurrences/%d/dwc", id),
                query = args, csv = csv, ...)
  return(res)
}

#' Field occurrences
#' @rdname tw_field_occurrences
#' @export
tw_fo <- tw_field_occurrences
