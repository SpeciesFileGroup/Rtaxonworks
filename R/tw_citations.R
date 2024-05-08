#' Citations
#'
#' @export
#' @template args
#' @param citation_id (integer, vector) filter by citation ID
#' @param citation_object_id (integer, vector) filter by citation object ID
#' @param citation_object_type (string, vector) filter by citation object type (AssertedDistribution, BiologicalAssociation, NomenclatureStatus, Otu, TaxonName, TaxonNameRelationship, TypeMaterial)
#' @param is_original (boolean) filter by original citation
#' @param model_id (string) filter by model ID
#' @param polymorphic (boolean) filter by polymorphic citation
#' @param source_id (integer) filter by source ID
#' @return list
#' @examples
#' \dontrun{
#' tw_citations()
#' }
tw_citations <- function(
    citation_id = NULL, citation_object_id = NULL,
    citation_object_type = NULL, is_original = NULL, model_id = NULL,
    polymorphic = NULL, source_id = NULL, csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    citation_id = citation_id, citation_object_id = citation_object_id,
    citation_object_type = citation_object_type, is_original = is_original, model_id = model_id,
    polymorphic = polymorphic, source_id = source_id, token = token, project_token = project_token,
    page = page, per = per
  ))

  vector_params <- c("citation_id", "citation_object_id", "citation_object_type", "source_id")

  res <- tw_GET(api_base_url(), "/citations", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
