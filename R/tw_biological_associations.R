#' Biological Associations
#'
#' @export
#' @importFrom tibble as_tibble
#' @param subresource (string) access API subresource (e.g., subresource=simple for the simple biological associations table)
#' @param anatomical_part_id (integer, vector) filter by anatomical part id
#' @param biological_association_id (integer, vector) filter by biological association id
#' @param biological_associations_graph_id (integer, vector) filter by biological associations graph id
#' @param biological_relationship_id (integer, vector) filter by biological relationship id
#' @param collecting_event_id (integer, vector) filter by collecting event id
#' @param collection_object_id (integer, vector) filter by collection object id
#' @param collection_object_as_subject_or_as_object (string) `subject`, `object`, or NULL for either
#' @param descendants (boolean) include descendants
#' @param exclude_taxon_name_relationship (boolean) exclude taxon name relationship
#' @param field_occurrence_id (integer, vector) filter by field occurrence id
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param geo_mode (string) geographic matching mode
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param geo_collecting_event_geographic_area (boolean) include the geographic area of related collecting events
#' @param object_biological_property_id (integer, vector) filter by object biological property id
#' @param object_object_global_id (integer, vector) filter by object global id
#' @param object_taxon_name_id (integer, vector) filter by object taxon name id
#' @param object_type (string) filter by object type (`Otu`, `CollectionObject`, `FieldOccurrence`)
#' @param otu_id (integer, vector) filter by otu id
#' @param radius (integer) buffer radius in meters for `geo_json`
#' @param subject_biological_property_id (integer, vector) filter by subject biological property id
#' @param subject_object_global_id (integer, vector) filter by subject global id
#' @param subject_taxon_name_id (integer, vector) filter by subject taxon name id
#' @param subject_type (string) filter by subject type (`Otu`, `CollectionObject`, `FieldOccurrence`)
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_id_mode (boolean) taxon_name_id match mode
#' @param wkt (string) filter by well known text shape
#' @param biological_association_object_id (integer, vector) polymorphic filter by object id
#' @param biological_association_object_type (string, vector) polymorphic filter by object type
#' @param biological_association_subject_id (integer, vector) polymorphic filter by subject id
#' @param biological_association_subject_type (string, vector) polymorphic filter by subject type
#' @param citations (boolean) filter by biological associations with citations
#' @param citation_documents (boolean) filter by biological associations with citation documents
#' @param origin_citation (boolean) filter by biological associations with an origin citation
#' @param confidences (boolean) filter by biological associations with confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude biological associations with matching confidences
#' @param depictions (boolean) filter by biological associations with depictions
#' @param data_depictions (boolean) filter by biological associations with data depictions
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by biological associations with images
#' @param data_attributes (boolean) filter by biological associations with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string, vector) filter by exact data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param identifier (string) filter by biological association identifier
#' @param identifier_end (string) filter by biological association identifier end
#' @param identifier_exact (boolean) filter by exact match on biological association identifier
#' @param identifier_start (string) filter by biological association identifier start
#' @param identifier_type (string, vector) filter by biological association identifier class type (e.g., "Identifier::Local::CatalogNumber")
#' @param identifiers (boolean) filter by biological associations with identifiers
#' @param global_identifiers (boolean) filter by biological associations with global identifiers
#' @param local_identifiers (boolean) filter by biological associations with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string of identifiers
#' @param match_identifiers_delimiter (string) the delimiter used to separate identifiers
#' @param match_identifiers_type (string) filter by internal or external identifier type
#' @param namespace_id (integer) filter by namespace id
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by biological associations with notes
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param tags (boolean) filter by biological associations with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_biological_associations()
#'
#' # How to get all simple biological associations with pagination
#' page <- 1
#' per <- 500
#' res <- tw_ba(subresource = "simple", page = page, per = per)
#' results <- res$data
#' while (page < res$meta$total_pages) {
#'   page <- page + 1
#'   res <- tw_ba(subresource = "simple", page = page, per = per)
#'   results <- rbind(results, res$data)
#' }
#' }
tw_biological_associations <- function(subresource = NULL,
  anatomical_part_id = NULL, biological_association_id = NULL,
  biological_associations_graph_id = NULL, biological_relationship_id = NULL,
  collecting_event_id = NULL, collection_object_id = NULL,
  collection_object_as_subject_or_as_object = NULL, descendants = NULL,
  exclude_taxon_name_relationship = NULL, field_occurrence_id = NULL,
  geo_json = NULL, geo_mode = NULL, geo_shape_id = NULL, geo_shape_type = NULL,
  geo_collecting_event_geographic_area = NULL,
  object_biological_property_id = NULL, object_object_global_id = NULL,
  object_taxon_name_id = NULL, object_type = NULL, otu_id = NULL, radius = NULL,
  subject_biological_property_id = NULL, subject_object_global_id = NULL,
  subject_taxon_name_id = NULL, subject_type = NULL, taxon_name_id = NULL,
  taxon_name_id_mode = NULL, wkt = NULL,
  biological_association_object_id = NULL, biological_association_object_type = NULL,
  biological_association_subject_id = NULL, biological_association_subject_type = NULL,
  citations = NULL, citation_documents = NULL, origin_citation = NULL,
  confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
  depictions = NULL, data_depictions = NULL, image_id = NULL, images = NULL,
  data_attributes = NULL, data_attribute_predicate_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_value = NULL,
  identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
  identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
  global_identifiers = NULL, local_identifiers = NULL, match_identifiers = NULL,
  match_identifiers_delimiter = NULL, match_identifiers_type = NULL,
  namespace_id = NULL, note_exact = NULL, note_text = NULL, notes = NULL,
  keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, exclude_tags = NULL,
  token = NULL, project_token = NULL, csv = TRUE, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    anatomical_part_id = anatomical_part_id,
    biological_association_id = biological_association_id,
    biological_associations_graph_id = biological_associations_graph_id,
    biological_relationship_id = biological_relationship_id,
    collecting_event_id = collecting_event_id,
    collection_object_id = collection_object_id,
    collection_object_as_subject_or_as_object = collection_object_as_subject_or_as_object,
    descendants = descendants,
    exclude_taxon_name_relationship = exclude_taxon_name_relationship,
    field_occurrence_id = field_occurrence_id,
    geo_json = geo_json, geo_mode = geo_mode,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    geo_collecting_event_geographic_area = geo_collecting_event_geographic_area,
    object_biological_property_id = object_biological_property_id,
    object_object_global_id = object_object_global_id,
    object_taxon_name_id = object_taxon_name_id,
    object_type = object_type, otu_id = otu_id, radius = radius,
    subject_biological_property_id = subject_biological_property_id,
    subject_object_global_id = subject_object_global_id,
    subject_taxon_name_id = subject_taxon_name_id,
    subject_type = subject_type, taxon_name_id = taxon_name_id,
    taxon_name_id_mode = taxon_name_id_mode, wkt = wkt,
    biological_association_object_id = biological_association_object_id,
    biological_association_object_type = biological_association_object_type,
    biological_association_subject_id = biological_association_subject_id,
    biological_association_subject_type = biological_association_subject_type,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    depictions = depictions, data_depictions = data_depictions,
    image_id = image_id, images = images,
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
    token = token, project_token = project_token,
    page = page, per = per))

  if (is.null(subresource)) {
    endpoint <- "/biological_associations"
  } else if (subresource == "extended") {
    endpoint <- "/biological_associations/extended"
  } else if (subresource == "simple") {
    endpoint <- "/biological_associations/simple"
  } else {
    endpoint <- "/biological_associations"
  }

  vector_params <- c("anatomical_part_id", "biological_association_id",
    "biological_associations_graph_id", "biological_relationship_id",
    "biological_association_object_id", "biological_association_object_type",
    "biological_association_subject_id", "biological_association_subject_type",
    "collecting_event_id", "collection_object_id", "confidence_level_id",
    "data_attribute_exact_value", "data_attribute_predicate_id",
    "data_attribute_value", "field_occurrence_id", "geo_shape_id", "geo_shape_type",
    "identifier_type", "image_id", "keyword_id_and", "keyword_id_or",
    "object_biological_property_id", "object_object_global_id",
    "object_taxon_name_id", "otu_id", "subject_biological_property_id",
    "subject_object_global_id", "subject_taxon_name_id", "taxon_name_id")

  res <- tw_GET(api_base_url(), endpoint, query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Biological Associations
#'
#' @rdname tw_biological_associations
#' @export
tw_ba <- tw_biological_associations
