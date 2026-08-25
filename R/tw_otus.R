#' Otus
#'
#' @export
#' @param id (integer) access an OTU by ID
#' @param subresource (string) access an API subresource (e.g., `inventory/dwc`)
#' @param otu_id (integer, vector) filter by otu id
#' @param name (string, vector) literal match against `Otu#name`
#' @param name_exact (boolean) require exact match on name
#' @param with_name (boolean) `TRUE`: has a name, `FALSE`: no name
#' @param taxon_name (string) filter by associated taxon name
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param genus_name (string) filter by genus name
#' @param descendants (boolean) include OTU descendants
#' @param ancestrify (boolean) also include coordinate OTUs for inferred ancestors
#' @param coordinatify (boolean) also include coordinate OTUs for the result
#' @param exclude_taxon_name_relationship (boolean) exclude via taxon name relationship
#' @param descriptor_id (integer, vector) filter by descriptor id
#' @param biological_association_id (integer, vector) filter by biological association id
#' @param biological_associations (boolean) filter by otus with biological associations
#' @param biological_relationship_id (integer, vector) filter by biological relationship id
#' @param collecting_event_id (integer, vector) filter by collecting event id
#' @param collection_objects (boolean) filter by otus with collection objects
#' @param common_names (boolean) filter by otus with common names
#' @param contents (boolean) filter by otus with contents
#' @param dwc_occurrences (boolean) filter by otus with Darwin Core occurrences
#' @param asserted_distributions (boolean) filter by otus with asserted distributions
#' @param observations (boolean) filter by otus with observations
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param geo_mode (string) geographic matching mode
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param radius (integer) buffer radius in meters for `geo_json`
#' @param wkt (string) filter by well-known text shape
#' @param citations (boolean) filter by otus with citations
#' @param citation_documents (boolean) filter by otus with citation documents
#' @param origin_citation (boolean) filter by otus with an origin citation
#' @param confidences (boolean) filter by otus with confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude otus with matching confidences
#' @param depictions (boolean) filter by otus with depictions
#' @param data_depictions (boolean) filter by otus with data depictions
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by otus with images
#' @param sound_id (integer, vector) filter by sound id (via conveyances)
#' @param sounds (boolean) filter by otus with sounds
#' @param conveyances (boolean) filter by otus with conveyances
#' @param data_attributes (boolean) filter by otus with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string) filter by exact data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param tags (boolean) filter by otus with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @param note_exact (boolean) require exact match on note text
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by otus with notes
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type
#' @param identifiers (boolean) filter by otus with identifiers
#' @param global_identifiers (boolean) filter by otus with global identifiers
#' @param local_identifiers (boolean) filter by otus with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string
#' @param match_identifiers_delimiter (string) delimiter for `match_identifiers`
#' @param match_identifiers_type (string) internal or identifier
#' @param namespace_id (integer) filter by namespace id
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_otus()
#' tw_otus(id = 1)
#' tw_otus(id = 1, subresource = "inventory/dwc")
#' }
tw_otus <- function(id = NULL, subresource = NULL,
  otu_id = NULL, name = NULL, name_exact = NULL, with_name = NULL,
  taxon_name = NULL, taxon_name_id = NULL, genus_name = NULL,
  descendants = NULL, ancestrify = NULL, coordinatify = NULL,
  exclude_taxon_name_relationship = NULL, descriptor_id = NULL,
  biological_association_id = NULL, biological_associations = NULL,
  biological_relationship_id = NULL, collecting_event_id = NULL,
  collection_objects = NULL, common_names = NULL, contents = NULL,
  dwc_occurrences = NULL, asserted_distributions = NULL, observations = NULL,
  geo_json = NULL, geo_mode = NULL, geo_shape_id = NULL, geo_shape_type = NULL,
  radius = NULL, wkt = NULL,
  citations = NULL, citation_documents = NULL, origin_citation = NULL,
  confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
  depictions = NULL, data_depictions = NULL, image_id = NULL, images = NULL,
  sound_id = NULL, sounds = NULL, conveyances = NULL,
  data_attributes = NULL, data_attribute_predicate_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_value = NULL,
  keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, exclude_tags = NULL,
  note_exact = NULL, note_text = NULL, notes = NULL,
  identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
  identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
  global_identifiers = NULL, local_identifiers = NULL, match_identifiers = NULL,
  match_identifiers_delimiter = NULL, match_identifiers_type = NULL,
  namespace_id = NULL,
  csv = TRUE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  # single-record and subresource paths do not support CSV
  if (!is.null(id)) csv <- FALSE
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    otu_id = otu_id, name = name, name_exact = name_exact,
    with_name = with_name, taxon_name = taxon_name,
    taxon_name_id = taxon_name_id, genus_name = genus_name,
    descendants = descendants, ancestrify = ancestrify,
    coordinatify = coordinatify,
    exclude_taxon_name_relationship = exclude_taxon_name_relationship,
    descriptor_id = descriptor_id,
    biological_association_id = biological_association_id,
    biological_associations = biological_associations,
    biological_relationship_id = biological_relationship_id,
    collecting_event_id = collecting_event_id,
    collection_objects = collection_objects, common_names = common_names,
    contents = contents, dwc_occurrences = dwc_occurrences,
    asserted_distributions = asserted_distributions,
    observations = observations,
    geo_json = geo_json, geo_mode = geo_mode,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    radius = radius, wkt = wkt,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    depictions = depictions, data_depictions = data_depictions,
    image_id = image_id, images = images,
    sound_id = sound_id, sounds = sounds, conveyances = conveyances,
    data_attributes = data_attributes,
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_value = data_attribute_value,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    tags = tags, exclude_tags = exclude_tags,
    note_exact = note_exact, note_text = note_text, notes = notes,
    identifier = identifier, identifier_end = identifier_end,
    identifier_exact = identifier_exact, identifier_start = identifier_start,
    identifier_type = identifier_type, identifiers = identifiers,
    global_identifiers = global_identifiers, local_identifiers = local_identifiers,
    match_identifiers = match_identifiers,
    match_identifiers_delimiter = match_identifiers_delimiter,
    match_identifiers_type = match_identifiers_type,
    namespace_id = namespace_id,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("biological_relationship_id", "collecting_event_id",
    "confidence_level_id", "data_attribute_predicate_id",
    "data_attribute_value", "descriptor_id", "geo_shape_id", "geo_shape_type",
    "identifier_type", "image_id", "keyword_id_and", "keyword_id_or",
    "name", "otu_id", "sound_id", "taxon_name_id")

  if (is.null(id)) {
    res <- tw_GET(api_base_url(), "/otus", query = args, csv = csv, vector_params = vector_params, ...)
  } else {
    if (is.null(subresource)) {
      res <- tw_GET(api_base_url(), sprintf("/otus/%d", id), query = args, csv = csv, vector_params = vector_params, ...)
    } else {
      res <- tw_GET(api_base_url(), sprintf("/otus/%d/%s", id, subresource), query = args, csv = csv, vector_params = vector_params, ...)
    }
  }
  return(res)
}


#' Otus
#'
#' @rdname tw_otus
#' @export
tw_otu <- tw_otus
