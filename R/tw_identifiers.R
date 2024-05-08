#' Identifiers
#'
#' @export
#' @template args
#' @param identifier (string, vector) filter by one or more identifiers
#' @param identifier_id (integer, vector) filter by one or more identifier IDs (e.g., the internal ID from the identifiers table in TaxonWorks)
#' @param identifier_object_id (integer, vector) filter by one or more object IDs (e.g., the internal ID from any related table in TaxonWorks)
#' @param identifier_object_type (string, vector) filter by one or more object types (e.g., Otu, CollectionObject, TaxonName, etc.)
#' @param namespace_id (integer, vector) filter by one or more namespace IDs
#' @param namespace_name (string) filter by a namespace name
#' @param namespace_short_name (string) filter by a namespace short name
#' @param object_global_id (string) filter by an object global ID
#' @param query_string (string) filter by a query string on the cached identifier (wildcards allowed)
#' @param type (string) filter by identifier type (e.g., Identifier::Local::CatalogNumber)
#' @param polymorphic (boolean) filter by polymorphic identifiers
#' @return list
#' @examples
#' \dontrun{
#' tw_identifiers(identifier_id=c(1375290,1375299,1375300))
#' }
tw_identifiers <- function(identifier = NULL, identifier_id = NULL, identifier_object_id = NULL,
    identifier_object_type = NULL, namespace_id = NULL, namespace_name = NULL,
    namespace_short_name = NULL, object_global_id = NULL, query_string = NULL,
    type = NULL, polymorphic = NULL, csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(identifier = identifier, identifier_id = identifier_id, identifier_object_id = identifier_object_id,
    identifier_object_type = identifier_object_type, namespace_id = namespace_id, namespace_name = namespace_name,
    namespace_short_name = namespace_short_name, object_global_id = object_global_id, query_string = query_string,
    type = type, polymorphic = polymorphic, token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("identifier", "identifier_id", "identifier_object_id", "identifier_object_type", "namespace_id")

  res <- tw_GET(api_base_url(), "/identifiers", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
