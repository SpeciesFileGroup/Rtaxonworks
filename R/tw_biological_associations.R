#' Biological Associations
#'
#' @export
#' @importFrom tibble as_tibble
#' @param subresource (string) access API subresource (e.g., subresource=simple for the simple biological associations table)
#' @param biological_association_id (integer) filter by biological association id
#' @param biological_associations_graph_id (integer) filter by biological associations graph id
#' @param biological_relationship_id (integer) filter by biological relationship id
#' @param collecting_event_id (integer) filter by collecting event id
#' @param collection_object_id (integer) filter by collection object id
#' @param descendants (boolean) include descendants
#' @param exclude_taxon_name_relationship (boolean) exclude taxon name relationship
#' @param geo_json (boolean) return result as geojson
#' @param geographic_area_id (integer) filter by geographic area id
#' @param geographic_area_mode (boolean)
#' @param object_biological_property_id (integer) filter by object biological property id
#' @param object_object_global_id (integer) filter by object object global id
#' @param object_taxon_name_id (integer) filter by object taxon name id
#' @param object_type (string) filter by object type (Otu, CollectionObject)
#' @param otu_id (integer) filter by otu id
#' @param subject_biological_property_id (integer) filter by subject biological property id
#' @param subject_object_global_id (integer) filter by subject object global id
#' @param subject_taxon_name_id (integer) filter by subject taxon name id
#' @param subject_type (string) filter by subject type (Otu, CollectionObject)
#' @param taxon_name_id (integer) filter by taxon name id
#' @param taxon_name_id_mode (boolean)
#' @param wkt (string) WKT shape to search for biological associations
#' @param citations (boolean) filter by biological associations with citations
#' @param citation_documents (boolean) filter by biological associations with citation documents
#' @param origin_citation (boolean) filter by biological associations with an origin citation
#' @param identifier (string) filter by biological association identifier
#' @param identifier_end (string) filter by biological association identifier end
#' @param identifier_exact (boolean) filter by exact match on biological association identifier
#' @param identifier_start (string) filter by biological association identifier start
#' @param identifier_type (string) filter by biological association identifier class type (e.g., "Identifier::Local::CatalogNumber")
#' @param identifiers (boolean) filter by biological associations with identifiers
#' @param local_identifiers (boolean) filter by biological associations with local identifiers
#' @param match_identifiers_delimiter (string) A list delimiter
#' @param match_identifiers_type (string) one of 'internal' or 'identifier', if 'internal' then references the internal id of the object
#' @param namespace_id (integer) filter by namespace id
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by biological associations with notes
#' @param keyword_id_and (integer) filter by keyword id with and operator
#' @param keyword_id_or (integer) filter by keyword id with or operator
#' @param tags (boolean) filter by biological associations with tags
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_biological_associations()
#'
#' # How to get all simple biological associations with pagination
#' page <- 1
#' per <- 500
#' results <- res <- tw_ba(subresource='simple', page=page, per=per)
#' while (nrow(res) > 0) {
#'   page <- page + 1
#'   res <- tw_ba(subresource='simple', page=page, per=per)
#'   results <- rbind(results, res)
#' }
#' }
tw_biological_associations <- function(subresource = NULL, biological_association_id = NULL, 
  biological_associations_graph_id = NULL, biological_relationship_id = NULL, 
  collecting_event_id = NULL, collection_object_id = NULL, descendants = NULL, 
  exclude_taxon_name_relationship = NULL, geo_json = NULL, 
  geographic_area_id = NULL, geographic_area_mode = NULL, 
  object_biological_property_id = NULL, object_object_global_id = NULL, 
  object_taxon_name_id = NULL, object_type = NULL, otu_id = NULL, 
  subject_biological_property_id = NULL, subject_object_global_id = NULL, 
  subject_taxon_name_id = NULL, subject_type = NULL, taxon_name_id = NULL, 
  taxon_name_id_mode = NULL, wkt = NULL, citations = NULL, 
  citation_documents = NULL, origin_citation = NULL, identifier = NULL, 
  identifier_end = NULL, identifier_exact = NULL, identifier_start = NULL, 
  identifier_type = NULL, identifiers = NULL, local_identifiers = NULL, 
  match_identifiers_delimiter = NULL, match_identifiers_type = NULL, 
  namespace_id = NULL, note_exact = NULL, note_text = NULL, notes = NULL, 
  keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, 
  token = NULL, project_token = NULL, csv = TRUE, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(biological_association_id = biological_association_id, 
    biological_associations_graph_id = biological_associations_graph_id, 
    biological_relationship_id = biological_relationship_id, 
    collecting_event_id = collecting_event_id, 
    collection_object_id = collection_object_id, 
    descendants = descendants, 
    exclude_taxon_name_relationship = exclude_taxon_name_relationship, 
    geo_json = geo_json, geographic_area_id = geographic_area_id, 
    geographic_area_mode = geographic_area_mode, 
    object_biological_property_id = object_biological_property_id, 
    object_object_global_id = object_object_global_id, 
    object_taxon_name_id = object_taxon_name_id, 
    object_type = object_type, otu_id = otu_id, 
    subject_biological_property_id = subject_biological_property_id, 
    subject_object_global_id = subject_object_global_id, 
    subject_taxon_name_id = subject_taxon_name_id, 
    subject_type = subject_type, taxon_name_id = taxon_name_id, 
    taxon_name_id_mode = taxon_name_id_mode, 
    wkt = wkt, citations = citations, citation_documents = citation_documents, 
    origin_citation = origin_citation, identifier = identifier, 
    identifier_end = identifier_end, identifier_exact = identifier_exact, 
    identifier_start = identifier_start, identifier_type = identifier_type, 
    identifiers = identifiers, local_identifiers = local_identifiers, 
    match_identifiers_delimiter = match_identifiers_delimiter, 
    match_identifiers_type = match_identifiers_type, 
    namespace_id = namespace_id, note_exact = note_exact, 
    note_text = note_text, notes = notes, 
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or, 
    tags = tags, token = token, project_token = project_token, 
    page = page, per = per))

  if (is.null(subresource)) {
    endpoint <- "/biological_associations"
  } else if (subresource == "simple") {
    endpoint <- "/biological_associations/simple"
  } else {
    endpoint <- "/biological_associations"
  }

  res <- tw_GET(api_base_url(), endpoint, query = args, ...)
  return(res)
}


#' Biological Associations
#'
#' @rdname tw_biological_associations
#' @export
tw_ba <- tw_biological_associations
