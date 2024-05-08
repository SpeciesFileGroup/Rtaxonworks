#' Observation matrices
#'
#' @export
#' @template args
#' @param observation_matrix_id (integer, vector) filter by observation matrix id
#' @return list
#' @examples
#' \dontrun{
#' tw_observation_matrices()
#' }
tw_observation_matrices <- function(observation_matrix_id = NULL, csv = FALSE, token = NULL, project_token = NULL,
                     page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(observation_matrix_id = observation_matrix_id, token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("observation_matrix_id")

  res <- tw_GET(api_base_url(), "/observation_matrices", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Observation matrices
#'
#' @rdname tw_observation_matrices
#' @export
tw_obsm <- tw_observation_matrices
