#' Biological relationships
#'
#' @export
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_biological_relationships()
#' }
tw_biological_relationships <- function(csv = FALSE, token = NULL, project_token = NULL,
                     page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token, page = page, per = per))

  vector_params <- c()

  res <- tw_GET(api_base_url(), "/biological_relationships", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Biological relationships
#'
#' @rdname tw_biological_relationships
#' @export
tw_br <- tw_biological_relationships
