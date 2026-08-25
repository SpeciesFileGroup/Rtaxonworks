#' Observations
#'
#' @export
#' @template args
#' @param character_state_id (integer) filter by character state ID
#' @param collection_object_id (integer, vector) filter by collection object ID
#' @param descendants (boolean) filter by descendants
#' @param descriptor_id (integer, vector) filter by descriptor ID
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param geo_mode (string) geographic matching mode
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param observation_id (integer, vector) filter by observation ID
#' @param observation_matrix_id (integer, vector) filter by observation matrix ID
#' @param observation_object_global_id (string) filter by observation object global ID
#' @param observation_object_id (integer, vector) filter by observation object id
#' @param observation_object_type (string, vector) filter by observation object type (`Extract`, `CollectionObject`, `Otu`, `FieldOccurrence`)
#' @param observation_type (string, vector) filter by observation type (e.g., `Observation::Qualitative`)
#' @param otu_id (integer, vector) filter by OTU ID
#' @param radius (integer) buffer radius in meters for `geo_json`
#' @param sound_id (integer, vector) filter by sound ID
#' @param taxon_name_id (integer, vector) filter by taxon name ID
#' @param wkt (string) filter by well-known text shape
#' @param citations (boolean) filter by observations with citations
#' @param citation_documents (boolean) filter by observations with citation documents
#' @param origin_citation (boolean) filter by observations with an origin citation
#' @param confidences (boolean) filter by observations with confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude observations with matching confidences
#' @param data_attributes (boolean) filter by observations with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string, vector) filter by exact data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param depictions (boolean) filter by observations with depictions
#' @param data_depictions (boolean) filter by observations with data depictions
#' @param image_id (integer, vector) filter by image ID
#' @param images (boolean) filter by observations with images
#' @param notes (boolean) filter by observations with notes
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param protocols (boolean) filter by observations with protocols
#' @param tags (boolean) filter by observations with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param identifier (string) filter by identifier
#' @param identifier_type (string, vector) filter by identifier class type
#' @param identifiers (boolean) filter by observations with identifiers
#' @return list
#' @examples
#' \dontrun{
#' tw_observations()
#' }
tw_observations <- function(
    character_state_id = NULL, collection_object_id = NULL, descendants = NULL,
    descriptor_id = NULL, geo_json = NULL, geo_mode = NULL,
    geo_shape_id = NULL, geo_shape_type = NULL,
    observation_id = NULL, observation_matrix_id = NULL,
    observation_object_global_id = NULL, observation_object_id = NULL,
    observation_object_type = NULL, observation_type = NULL,
    otu_id = NULL, radius = NULL, sound_id = NULL, taxon_name_id = NULL, wkt = NULL,
    citations = NULL, citation_documents = NULL, origin_citation = NULL,
    confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
    data_attributes = NULL, data_attribute_predicate_id = NULL,
    data_attribute_exact_value = NULL, data_attribute_value = NULL,
    depictions = NULL, data_depictions = NULL, image_id = NULL, images = NULL,
    notes = NULL, note_text = NULL, note_exact = NULL,
    protocols = NULL, tags = NULL, exclude_tags = NULL,
    keyword_id_and = NULL, keyword_id_or = NULL,
    identifier = NULL, identifier_type = NULL, identifiers = NULL,
    csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    character_state_id = character_state_id,
    collection_object_id = collection_object_id,
    descendants = descendants, descriptor_id = descriptor_id,
    geo_json = geo_json, geo_mode = geo_mode,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    observation_id = observation_id,
    observation_matrix_id = observation_matrix_id,
    observation_object_global_id = observation_object_global_id,
    observation_object_id = observation_object_id,
    observation_object_type = observation_object_type,
    observation_type = observation_type, otu_id = otu_id, radius = radius,
    sound_id = sound_id, taxon_name_id = taxon_name_id, wkt = wkt,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    data_attributes = data_attributes,
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_value = data_attribute_value,
    depictions = depictions, data_depictions = data_depictions,
    image_id = image_id, images = images,
    notes = notes, note_text = note_text, note_exact = note_exact,
    protocols = protocols, tags = tags, exclude_tags = exclude_tags,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    identifier = identifier, identifier_type = identifier_type,
    identifiers = identifiers,
    token = token, project_token = project_token, page = page, per = per
  ))

  vector_params <- c(
    "collection_object_id", "confidence_level_id",
    "data_attribute_exact_value", "data_attribute_predicate_id",
    "data_attribute_value", "descriptor_id", "geo_shape_id", "geo_shape_type",
    "identifier_type", "image_id", "keyword_id_and", "keyword_id_or",
    "observation_id", "observation_matrix_id", "observation_object_id",
    "observation_object_type", "observation_type", "otu_id", "sound_id",
    "taxon_name_id"
  )

  res <- tw_GET(api_base_url(), "/observations", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Observations
#'
#' @rdname tw_observations
#' @export
tw_obs <- tw_observations
