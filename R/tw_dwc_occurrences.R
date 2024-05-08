#' Darwin Core occurrences
#'
#' @export
#' @template args
#' @param dwc_occurrence_id (integer, vector) filter by Darwin Core occurrence ID
#' @param dwc_occurrence_object_id (integer, vector) filter by Darwin Core occurrence object ID
#' @param dwc_occurrence_object_type (string, vector) filter by Darwin Core occurrence object type
#' @return list
#' @examples
#' \dontrun{
#' tw_people(last_name = "Smith")
#' }
tw_dwc_occurrences <- function(
    dwc_occurrence_id = NULL, dwc_occurrence_object_id = NULL,
    dwc_occurrence_object_type = NULL, csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    dwc_occurrence_id = dwc_occurrence_id, dwc_occurrence_object_id = dwc_occurrence_object_id,
    dwc_occurrence_object_type = dwc_occurrence_object_type, token = token, project_token = project_token,
    page = page, per = per
  ))

  vector_params <- c("dwc_occurrence_id", "dwc_occurrence_object_id", "dwc_occurrence_object_type")

  res <- tw_GET(api_base_url(), "/dwc_occurrences", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Darwin Core occurrences
#'
#' @rdname tw_dwc_occurrences
#' @export
tw_dwc <- tw_dwc_occurrences
