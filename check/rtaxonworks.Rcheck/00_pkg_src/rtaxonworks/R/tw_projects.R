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
  res <- tw_GET(api_base_url())
  df <- as_tibble(tw_list_to_df(res$open_projects))
  return(df)
}
