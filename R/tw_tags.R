#' Tags
#'
#' @export
#' @template args
#' @param tag_id (integer, vector) filter by tag id
#' @param tag_object_id (integer, vector) filter by tag object id
#' @param tag_object_type (string, vector) filter by tag object type
#' @return list
#' @examples
#' \dontrun{
#' tw_tags()
#' }
tw_tags <- function(tag_id = NULL, tag_object_id = NULL,
                    tag_object_type = NULL, csv = FALSE, token = NULL, project_token = NULL,
                    page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(tag_id = tag_id, tag_object_id = tag_object_id,
                  tag_object_type = tag_object_type, token = token,
                  project_token = project_token, page = page, per = per))

  vector_params <- c("tag_id", "tag_object_id", "tag_object_type")

  res <- tw_GET(api_base_url(), "/tags", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Tags
#'
#' @rdname tw_tags
#' @export
tw_tag <- tw_tags
