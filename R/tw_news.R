#' News
#'
#' News items associated with the TaxonWorks instance or a project (e.g.,
#' `News::Project::BlogPost`).
#'
#' @export
#' @template args
#' @param id (integer) access a News item by ID
#' @param news_id (integer, vector) filter by News item id
#' @param type (string, vector) filter by news type (e.g., `News::Project::BlogPost`)
#' @return list
#' @examples
#' \dontrun{
#' tw_news()
#' }
tw_news <- function(id = NULL, news_id = NULL, type = NULL, csv = FALSE,
                    token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    news_id = news_id, type = type,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("news_id", "type")

  path <- if (is.null(id)) "/news" else sprintf("/news/%d", id)

  res <- tw_GET(api_base_url(), path, query = args, csv = csv,
                vector_params = vector_params, ...)
  return(res)
}
