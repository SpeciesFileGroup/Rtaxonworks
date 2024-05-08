#' Common names
#'
#' @export
#' @template args
#' @param common_name_id (integer, vector) filter by common name id
#' @param geographic_area_id (integer) filter by geographic area id
#' @param language_id (integer) filter by language id
#' @param name (string) filter by name
#' @param otu_id (integer, vector) filter by otu id
#' @return list
#' @examples
#' \dontrun{
#' tw_common_names()
#' }
tw_common_names <- function(common_name_id = NULL, geographic_area_id = NULL,
                            language_id = NULL, name = NULL, otu_id = NULL,
                            csv = FALSE, token = NULL, project_token = NULL,
                            page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    common_name_id = common_name_id, geographic_area_id = geographic_area_id,
    language_id = language_id, name = name, otu_id = otu_id,
    token = token, project_token = project_token, page = page, per = per
  ))

  vector_params <- c("common_name_id", "otu_id")

  res <- tw_GET(api_base_url(), "/common_names", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}

#' Common names
#'
#' @rdname tw_common_names
#' @export
tw_cn <- tw_common_names
