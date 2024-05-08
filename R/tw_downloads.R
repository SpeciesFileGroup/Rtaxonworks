#' Downloads
#'
#' @export
#' @template args
#' @param download_id (integer, vector) filter by download id
#' @param download_type (string, vector) filter by download type
#' @return list
#' @examples
#' \dontrun{
#' tw_downloads()
#' }
tw_downloads <- function(download_id = NULL, download_type = NULL, csv = FALSE, 
  token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(download_id = download_id, download_type = download_type, 
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("download_id", "download_type")

  res <- tw_GET(api_base_url(), "/downloads", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}

#' Downloads
#'
#' @rdname tw_downloads
#' @export
tw_download <- tw_downloads
