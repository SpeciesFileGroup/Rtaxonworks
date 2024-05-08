#' Both authenticated
#'
#' @export
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_both_authenticated()
#' }
tw_both_authenticated <- function(csv = FALSE, token = NULL, project_token = NULL,
                                  page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token, page = page, per = per))

  vector_params <- c()

  res <- tw_GET(api_base_url(), "/both_authenticated", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}

#' Project authenticated
#'
#' @export
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_project_authenticated()
#' }
tw_project_authenticated <- function(csv = FALSE, token = NULL, project_token = NULL,
                                     page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token, page = page, per = per))

  vector_params <- c()

  res <- tw_GET(api_base_url(), "/project_authenticated", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}

#' User authenticated
#'
#' @export
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_user_authenticated()
#' }
tw_user_authenticated <- function(csv = FALSE, token = NULL, project_token = NULL,
                                  page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token, page = page, per = per))

  vector_params <- c()

  res <- tw_GET(api_base_url(), "/user_authenticated", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
