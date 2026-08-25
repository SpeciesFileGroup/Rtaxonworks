#' Asserted distributions
#'
#' @export
#' @importFrom tibble as_tibble
#' @param asserted_distribution_id (integer, vector) filter by asserted distribution id
#' @param asserted_distribution_object_id (integer, vector) filter by asserted distribution object id
#' @param asserted_distribution_object_type (string, vector) filter by asserted distribution object type (e.g., `Otu`)
#' @param asserted_distribution_shape_type (string, vector) filter by shape type (`GeographicArea`, `Gazetteer`)
#' @param biological_association_id (integer, vector) filter by biological association id
#' @param descendants (boolean) include descendants
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param geo_mode (string) geographic matching mode
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param geographic_area_id (integer, vector) filter by geographic area id (legacy; prefer geo_shape_id)
#' @param geographic_area_mode (boolean) legacy geographic area matching mode
#' @param otu_id (integer, vector) filter by otu id
#' @param presence (boolean) filter by presence
#' @param radius (integer) filter by meters around shape boundary
#' @param source_id (integer, vector) filter by source id
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param wkt (string) filter by well known text
#' @param citations (boolean) filter by asserted distributions with citations
#' @param citation_documents (boolean) filter by asserted distributions with citation documents
#' @param origin_citation (boolean) filter by asserted distributions with an origin citation
#' @param confidences (boolean) filter by asserted distributions with confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude asserted distributions with matching confidences
#' @param data_attributes (boolean) filter by asserted distributions with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string, vector) filter by exact data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type (e.g., "Identifier::Local::CatalogNumber")
#' @param identifiers (boolean) filter by asserted distributions with identifiers
#' @param global_identifiers (boolean) filter by asserted distributions with global identifiers
#' @param local_identifiers (boolean) filter by asserted distributions with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string of identifiers
#' @param match_identifiers_delimiter (string) the delimiter used to separate identifiers
#' @param match_identifiers_type (string) filter by internal or external identifier type
#' @param namespace_id (integer) filter by namespace id
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by asserted distributions with notes
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param tags (boolean) filter by asserted distributions with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_asserted_distributions()
#' }
tw_asserted_distributions <- function(asserted_distribution_id = NULL,
    asserted_distribution_object_id = NULL, asserted_distribution_object_type = NULL,
    asserted_distribution_shape_type = NULL, biological_association_id = NULL,
    descendants = NULL, geo_json = NULL, geo_mode = NULL,
    geo_shape_id = NULL, geo_shape_type = NULL,
    geographic_area_id = NULL, geographic_area_mode = NULL,
    otu_id = NULL, presence = NULL, radius = NULL, source_id = NULL,
    taxon_name_id = NULL, wkt = NULL,
    citations = NULL, citation_documents = NULL, origin_citation = NULL,
    confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
    data_attributes = NULL, data_attribute_predicate_id = NULL,
    data_attribute_exact_value = NULL, data_attribute_value = NULL,
    identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
    identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
    global_identifiers = NULL, local_identifiers = NULL,
    match_identifiers = NULL, match_identifiers_delimiter = NULL,
    match_identifiers_type = NULL, namespace_id = NULL,
    note_exact = NULL, note_text = NULL, notes = NULL,
    keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, exclude_tags = NULL,
    csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(asserted_distribution_id = asserted_distribution_id,
    asserted_distribution_object_id = asserted_distribution_object_id,
    asserted_distribution_object_type = asserted_distribution_object_type,
    asserted_distribution_shape_type = asserted_distribution_shape_type,
    biological_association_id = biological_association_id,
    descendants = descendants, geo_json = geo_json, geo_mode = geo_mode,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    geographic_area_id = geographic_area_id,
    geographic_area_mode = geographic_area_mode, otu_id = otu_id,
    presence = presence, radius = radius, source_id = source_id,
    taxon_name_id = taxon_name_id, wkt = wkt,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    data_attributes = data_attributes,
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_value = data_attribute_value,
    identifier = identifier, identifier_end = identifier_end,
    identifier_exact = identifier_exact, identifier_start = identifier_start,
    identifier_type = identifier_type, identifiers = identifiers,
    global_identifiers = global_identifiers, local_identifiers = local_identifiers,
    match_identifiers = match_identifiers,
    match_identifiers_delimiter = match_identifiers_delimiter,
    match_identifiers_type = match_identifiers_type,
    namespace_id = namespace_id, note_exact = note_exact,
    note_text = note_text, notes = notes,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    tags = tags, exclude_tags = exclude_tags,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("asserted_distribution_id", "asserted_distribution_object_id",
    "asserted_distribution_object_type", "asserted_distribution_shape_type",
    "biological_association_id", "confidence_level_id",
    "data_attribute_exact_value", "data_attribute_predicate_id",
    "data_attribute_value", "geo_shape_id", "geo_shape_type",
    "geographic_area_id", "identifier_type", "keyword_id_and", "keyword_id_or",
    "otu_id", "source_id", "taxon_name_id")

  res <- tw_GET(api_base_url(), "/asserted_distributions", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Asserted distributions
#'
#' @rdname tw_asserted_distributions
#' @export
tw_ad <- tw_asserted_distributions
