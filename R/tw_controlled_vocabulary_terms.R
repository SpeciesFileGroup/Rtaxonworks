#' Controlled vocabulary terms
#'
#' @export
#' @importFrom tibble as_tibble
#' @param type (string) filter by type (BiocurationClass, BiologicalProperty, Keyword, Predicate, Topic)
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_controlled_vocabulary_terms()
#' }
tw_controlled_vocabulary_terms <- function(type = NULL, csv = FALSE, token = NULL, project_token = NULL, 
    page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(type = type, token = token, project_token = project_token, page = page, per = per))

  vector_params <- c()

  res <- tw_GET(api_base_url(), "/controlled_vocabulary_terms", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Controlled vocabulary terms
#'
#' @rdname tw_controlled_vocabulary_terms
#' @export
tw_vocab <- tw_controlled_vocabulary_terms


#' Controlled vocabulary terms
#'
#' @rdname tw_controlled_vocabulary_terms
#' @export
tw_cvt <- tw_controlled_vocabulary_terms
