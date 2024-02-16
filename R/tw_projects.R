#' Projects
#'
#' @export
#' @importFrom tibble as_tibble
#' @return a list of open projects
#' @examples
#' \dontrun{
#' tw_projects()
#' }
tw_projects <- function() {
  res <- tw_GET(api_base_url(), csv = FALSE)
  df <- as_tibble(res$open_projects)
  return(df)
}
