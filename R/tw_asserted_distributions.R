#' Asserted distributions
#'
#' @export
#' @importFrom tibble as_tibble
#' @param asserted_distribution_id (integer, vector) filter by asserted distribution id
#' @param descendants (boolean) include descendants
#' @param geo_json (boolean) filter by geojson
#' @param geographic_area_id (integer, vector) filter by geographic area id
#' @param geographic_area_mode (boolean)
#' @param otu_id (integer, vector) filter by otu id
#' @param presence (boolean) filter by presence
#' @param radius (integer) filter by meters around shape boundary
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param wkt (string) filter by well known text
#' @param citations (boolean) filter by asserted distributions with citations
#' @param citation_documents (boolean) filter by asserted distributions with citation documents
#' @param origin_citation (boolean) filter by asserted distributions with an origin citation
#' @param data_attribute_exact_value (boolean) require exact match on data attribute value
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param data_attributes (boolean) filter by asserted distributions with data attributes
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type (e.g., "Identifier::Local::CatalogNumber")
#' @param identifiers (boolean) filter by asserted distributions with identifiers
#' @param local_identifiers (boolean) filter by asserted distributions with local identifiers
#' @param match_identifiers_delimiter (string) the delimiter used to separate identifiers
#' @param match_identifiers_type (string) filter by internal or external identifier type
#' @param namespace_id (integer) filter by namespace id
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param tags (boolean) filter by asserted distributions with tags
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_asserted_distributions()
#' }
tw_asserted_distributions <- function(csv = FALSE, token = NULL, project_token = NULL, 
    page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("asserted_distribution_id", "data_attribute_predicate_id",
                     "data_attribute_value", "geographic_area_id", "identifier_type",
                     "keyword_id_and", "keyword_id_or", "otu_id", "taxon_name_id")

  res <- tw_GET(api_base_url(), "/asserted_distributions", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Asserted distributions
#'
#' @rdname tw_asserted_distributions
#' @export
tw_ad <- tw_asserted_distributions