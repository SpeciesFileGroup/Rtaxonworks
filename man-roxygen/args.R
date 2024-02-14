#' @param page (integer) requested number of offset records (Default: 0)
#' @param per (integer) requested number of maximum records to be returned (Default: 50, Maximum: 10000)
#' @param token (string) a user token (set with the TW_USER_TOKEN environment variable)
#' @param project_token (string) a project token (set with the TW_PROJECT_TOKEN environment variable)
#' @param ... curl options passed on to \code{\link[httr2]{verb-GET}}
