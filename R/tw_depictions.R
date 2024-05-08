#' Depictions
#'
#' @export
#' @template args
#' @param depiction_id (integer, vector) filter by depiction id
#' @param depiction_object_id (integer, vector) filter by depiction object id
#' @param depiction_object_type (string, vector) filter by depiction object type
#' @param image_id (integer, vector) filter by image id
#' @return list
#' @examples
#' \dontrun{
#' tw_depictions()
#' }
tw_depictions <- function(
    depiction_id = NULL, depiction_object_id = NULL,
    depiction_object_type = NULL, image_id = NULL, csv = FALSE, token = NULL,
    project_token = NULL, page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    depiction_id = depiction_id, depiction_object_id = depiction_object_id,
    depiction_object_type = depiction_object_type, image_id = image_id,
    token = token, project_token = project_token, page = page, per = per
  ))

  vector_params <- c("depiction_id", "depiction_object_id", "depiction_object_type", "image_id")

  res <- tw_GET(api_base_url(), "/depictions", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
