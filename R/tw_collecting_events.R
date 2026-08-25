#' Collecting events
#'
#' @export
#' @template args
#' @param collecting_event_id (integer, vector) filter by collecting event id
#' @param collecting_event_object_id (integer) polymorphic filter by object id
#' @param collection_object_id (integer, vector) filter by collection object id
#' @param otu_id (integer, vector) filter by otu id
#' @param collectors (boolean) filter by collecting events with a collector role
#' @param collector_id (integer, vector) filter by collector id
#' @param collector_id_all (boolean) require all listed collector ids to match
#' @param collector_id_or (integer, vector) filter by collector id (OR)
#' @param collection_objects (boolean) filter by collecting events with collection objects
#' @param georeferences (boolean) filter by collecting events with georeferences
#' @param geographic_area (boolean) filter by collecting events with a geographic area
#' @param geographic_area_id (integer, vector) filter by geographic area id (legacy; prefer geo_shape_id)
#' @param geo_collecting_event_geographic_area (boolean) include the geographic area of related collecting events
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param geo_mode (string) geographic matching mode
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param wkt (string) filter by well-known text describing the search area
#' @param wkt_geometry_type (string) restrict wkt filter to a geometry type
#' @param radius (integer) buffer radius in meters
#' @param meta_prioritize_geographic_area (string)
#' @param in_labels (string) wildcard-wrapped search against any label
#' @param in_verbatim_locality (string) wildcard-wrapped search against verbatim locality
#' @param md5_verbatim_label (string) filter by md5 hash of verbatim label
#' @param md5_of_verbatim_label (string) alias for md5_verbatim_label
#' @param start_date (string) filter by start date (yyyy-mm-dd)
#' @param end_date (string) filter by end date (yyyy-mm-dd)
#' @param partial_overlap_dates (boolean) allow partial date-range overlap
#' @param start_date_day (integer)
#' @param start_date_month (integer)
#' @param start_date_year (integer)
#' @param end_date_day (integer)
#' @param end_date_month (integer)
#' @param end_date_year (integer)
#' @param time_start_hour (integer)
#' @param time_start_minute (integer)
#' @param time_start_second (integer)
#' @param time_end_hour (integer)
#' @param time_end_minute (integer)
#' @param time_end_second (integer)
#' @param use_max (boolean)
#' @param use_min (boolean)
#' @param maximum_elevation (numeric) filter by maximum elevation in meters
#' @param minimum_elevation (numeric) filter by minimum elevation in meters
#' @param elevation_precision (numeric)
#' @param min_ma (numeric)
#' @param formation (string)
#' @param group (string)
#' @param lithology (string)
#' @param member (string)
#' @param field_notes (string)
#' @param document_label (string)
#' @param print_label (string)
#' @param verbatim_collectors (string)
#' @param verbatim_date (string)
#' @param verbatim_datum (string)
#' @param verbatim_elevation (string)
#' @param verbatim_geolocation_uncertainty (numeric)
#' @param verbatim_habitat (string)
#' @param verbatim_label (string)
#' @param verbatim_latitude (numeric)
#' @param verbatim_locality (string)
#' @param verbatim_longitude (numeric)
#' @param verbatim_method (string)
#' @param verbatim_trip_identifier (string)
#' @param citations (boolean) filter by collecting events with citations
#' @param citation_documents (boolean) filter by collecting events with citation documents
#' @param origin_citation (boolean) filter by collecting events with origin citations
#' @param confidences (boolean) filter by collecting events with confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude collecting events with matching confidences
#' @param protocols (boolean) filter by collecting events with protocols
#' @param protocol_id_and (integer, vector) match all listed protocol ids
#' @param protocol_id_or (integer, vector) match any of the listed protocol ids
#' @param depictions (boolean) filter by collecting events with depictions
#' @param data_depictions (boolean) filter by collecting events with data depictions
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by collecting events with images
#' @param data_attributes (boolean) filter by collecting events with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string) filter by data attribute exact value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param any_value_attribute (string, vector) filter by any wildcard-matched attribute value
#' @param wildcard_attribute (string, vector) fields to search when using `any_value_attribute`
#' @param identifier (string) filter by cached identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier type
#' @param identifiers (boolean) filter by collecting events with identifiers
#' @param global_identifiers (boolean) filter by collecting events with global identifiers
#' @param local_identifiers (boolean) filter by collecting events with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string of identifiers
#' @param match_identifiers_delimiter (string) the delimiter for the match_identifiers parameter
#' @param match_identifiers_type (string) internal or identifier
#' @param namespace_id (integer) filter by namespace id
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by collecting events with notes
#' @param keyword_id_and (integer, vector) filter by keyword id (AND)
#' @param keyword_id_or (integer, vector) filter by keyword id (OR)
#' @param tags (boolean) filter by collecting events with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @return list
#' @examples
#' \dontrun{
#' tw_collecting_events()
#' }
tw_collecting_events <- function(
  collecting_event_id = NULL, collecting_event_object_id = NULL,
  collection_object_id = NULL, otu_id = NULL,
  collectors = NULL, collector_id = NULL, collector_id_all = NULL,
  collector_id_or = NULL, collection_objects = NULL, georeferences = NULL,
  geographic_area = NULL, geographic_area_id = NULL,
  geo_collecting_event_geographic_area = NULL, geo_json = NULL, geo_mode = NULL,
  geo_shape_id = NULL, geo_shape_type = NULL, wkt = NULL,
  wkt_geometry_type = NULL, radius = NULL,
  meta_prioritize_geographic_area = NULL,
  in_labels = NULL, in_verbatim_locality = NULL,
  md5_verbatim_label = NULL, md5_of_verbatim_label = NULL,
  start_date = NULL, end_date = NULL, partial_overlap_dates = NULL,
  start_date_day = NULL, start_date_month = NULL, start_date_year = NULL,
  end_date_day = NULL, end_date_month = NULL, end_date_year = NULL,
  time_start_hour = NULL, time_start_minute = NULL, time_start_second = NULL,
  time_end_hour = NULL, time_end_minute = NULL, time_end_second = NULL,
  use_max = NULL, use_min = NULL,
  maximum_elevation = NULL, minimum_elevation = NULL, elevation_precision = NULL,
  min_ma = NULL, formation = NULL, group = NULL, lithology = NULL, member = NULL,
  field_notes = NULL, document_label = NULL, print_label = NULL,
  verbatim_collectors = NULL, verbatim_date = NULL, verbatim_datum = NULL,
  verbatim_elevation = NULL, verbatim_geolocation_uncertainty = NULL,
  verbatim_habitat = NULL, verbatim_label = NULL, verbatim_latitude = NULL,
  verbatim_locality = NULL, verbatim_longitude = NULL, verbatim_method = NULL,
  verbatim_trip_identifier = NULL,
  citations = NULL, citation_documents = NULL, origin_citation = NULL,
  confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
  protocols = NULL, protocol_id_and = NULL, protocol_id_or = NULL,
  depictions = NULL, data_depictions = NULL, image_id = NULL, images = NULL,
  data_attributes = NULL, data_attribute_predicate_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_value = NULL,
  any_value_attribute = NULL, wildcard_attribute = NULL,
  identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
  identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
  global_identifiers = NULL, local_identifiers = NULL, match_identifiers = NULL,
  match_identifiers_delimiter = NULL, match_identifiers_type = NULL,
  namespace_id = NULL, note_exact = NULL, note_text = NULL, notes = NULL,
  keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, exclude_tags = NULL,
  token = NULL, project_token = NULL,
  csv = FALSE, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    collecting_event_id = collecting_event_id,
    collecting_event_object_id = collecting_event_object_id,
    collection_object_id = collection_object_id, otu_id = otu_id,
    collectors = collectors, collector_id = collector_id,
    collector_id_all = collector_id_all, collector_id_or = collector_id_or,
    collection_objects = collection_objects, georeferences = georeferences,
    geographic_area = geographic_area, geographic_area_id = geographic_area_id,
    geo_collecting_event_geographic_area = geo_collecting_event_geographic_area,
    geo_json = geo_json, geo_mode = geo_mode,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    wkt = wkt, wkt_geometry_type = wkt_geometry_type, radius = radius,
    meta_prioritize_geographic_area = meta_prioritize_geographic_area,
    in_labels = in_labels, in_verbatim_locality = in_verbatim_locality,
    md5_verbatim_label = md5_verbatim_label,
    md5_of_verbatim_label = md5_of_verbatim_label,
    start_date = start_date, end_date = end_date,
    partial_overlap_dates = partial_overlap_dates,
    start_date_day = start_date_day, start_date_month = start_date_month,
    start_date_year = start_date_year, end_date_day = end_date_day,
    end_date_month = end_date_month, end_date_year = end_date_year,
    time_start_hour = time_start_hour, time_start_minute = time_start_minute,
    time_start_second = time_start_second, time_end_hour = time_end_hour,
    time_end_minute = time_end_minute, time_end_second = time_end_second,
    use_max = use_max, use_min = use_min,
    maximum_elevation = maximum_elevation, minimum_elevation = minimum_elevation,
    elevation_precision = elevation_precision, min_ma = min_ma,
    formation = formation, group = group, lithology = lithology, member = member,
    field_notes = field_notes, document_label = document_label,
    print_label = print_label, verbatim_collectors = verbatim_collectors,
    verbatim_date = verbatim_date, verbatim_datum = verbatim_datum,
    verbatim_elevation = verbatim_elevation,
    verbatim_geolocation_uncertainty = verbatim_geolocation_uncertainty,
    verbatim_habitat = verbatim_habitat, verbatim_label = verbatim_label,
    verbatim_latitude = verbatim_latitude, verbatim_locality = verbatim_locality,
    verbatim_longitude = verbatim_longitude, verbatim_method = verbatim_method,
    verbatim_trip_identifier = verbatim_trip_identifier,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    protocols = protocols, protocol_id_and = protocol_id_and,
    protocol_id_or = protocol_id_or,
    depictions = depictions, data_depictions = data_depictions,
    image_id = image_id, images = images,
    data_attributes = data_attributes,
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_value = data_attribute_value,
    any_value_attribute = any_value_attribute,
    wildcard_attribute = wildcard_attribute,
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
    token = token, project_token = project_token, page = page, per = per
  ))

  vector_params <- c("any_value_attribute", "collecting_event_id",
    "collection_object_id", "collector_id", "collector_id_or",
    "confidence_level_id", "data_attribute_predicate_id",
    "data_attribute_value", "geo_shape_id", "geo_shape_type",
    "geographic_area_id", "identifier_type", "image_id", "keyword_id_and",
    "keyword_id_or", "otu_id", "protocol_id_and", "protocol_id_or",
    "wildcard_attribute")

  df <- tw_GET(api_base_url(), "/collecting_events", query = args, csv = csv, vector_params = vector_params, ...)
  return(df)
}

#' Collection events
#' @rdname tw_collecting_events
#' @export
tw_ce <- tw_collecting_events
