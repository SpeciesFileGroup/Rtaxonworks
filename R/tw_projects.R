#' Projects
#'
#' @export
#' @return list with two slots
#' - `result` (data.frame/tibble): results, a zero row data.frame
#' if no results found
#' - `meta` (data.frame/tibble): number of results found
#' @examples
#' tw_projects()
tw_projects <- function() {
  tmp <- tw_GET(api_base_url())
  tmp$result <- tibble::as_tibble(tmp$result)
  tmp <- tw_meta(tmp)
  return(tmp)
}
