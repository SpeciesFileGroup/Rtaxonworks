#' Projects
#'
#' @export
#' @return a list of open projects
#' @examples
#' \dontrun{
#' tw_projects()
#' }
tw_projects <- function() {
  res <- tw_GET(api_base_url(), csv = FALSE)
  return(res$open_projects)
}
