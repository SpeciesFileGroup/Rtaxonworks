#' Collecting events
#'
#' @export
#' @template args
#' @param citation_documents (boolean) filter by collecting events with citation documents
#' @param citations (boolean) filter by collecting events with citations
#' @param collection_objects (boolean) filter by collecting events with collection objects
#' @param collector_id (integer, vector) filter by collector id
#' @param collector_id_or (integer, vector) filter by collector id (OR)
#' @param collectors (boolean) filter by collecting events with collectors
#' @param data_attribute_exact_value (string) filter by data attribute exact value
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param data_attributes (boolean) filter by collecting events with data attributes
#' @param document_label (string) 
#' @param elevation_precision (numeric) 
#' @param end_date (string) filter by end date
#' @param end_date_day (integer) 
#' @param end_date_month (integer) 
#' @param end_date_year (integer) 
#' @param field_notes (string) 
#' @param formation (string) 
#' @param geographic_area (boolean) filter by collecting events with geographic area
#' @param geographic_area_id (integer, vector) filter by geographic area id
#' @param geographic_area_mode (boolean) 
#' @param georeferences (boolean) filter by collecting events with georeferences
#' @param group (string) 
#' @param identifier (string) filter by cached identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier type
#' @param identifiers (boolean) filter by collecting events with identifiers
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by collecting events with images
#' @param in_labels (string) wildcard wrapped search against any label
#' @param in_verbatim_locality (string) wildcard wrapped search against verbatim locality
#' @param keyword_id_and (integer, vector) filter by keyword id (AND)
#' @param keyword_id_or (integer, vector) filter by keyword id (OR)
#' @param lithology (string) 
#' @param local_identifiers (boolean) filter by collecting events with local identifiers
#' @param match_identifiers_delimiter (string) the delimiter for the match_identifiers parameter (e.g., comma (,), new line (\n), tab (\t), etc.)
#' @param match_identifiers_type (string) the type of match to perform on the identifiers (internal or identifier)
#' @param maximum_elevation (numeric) filter by maximum elevation in meters
#' @param md5_of_verbatim_label (string) 
#' @param md5_verbatim_label (string) filter by md5 hash of verbatim label
#' @param member (string) 
#' @param meta_prioritize_geographic_area (string) 
#' @param min_ma (numeric) 
#' @param minimum_elevation (numeric) filter by minimum elevation in meters
#' @param namespace_id (integer) filter by namespace id
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by collecting events with notes
#' @param origin_citation (boolean) filter by collecting events with origin citations
#' @param otu_id (integer, vector) filter by otu id
#' @param print_label (string) 
#' @param radius (integer) filter by radius in meters
#' @param recent (boolean) filter by recent collecting events
#' @param start_date (string) filter by start date
#' @param start_date_day (integer) 
#' @param start_date_month (integer) 
#' @param start_date_year (integer) 
#' @param tags (boolean) filter by collecting events with tags
#' @param time_end_hour (integer) 
#' @param time_end_minute (integer) 
#' @param time_end_second (integer) 
#' @param time_start_hour (integer) 
#' @param time_start_minute (integer) 
#' @param time_start_second (integer) 
#' @param token (NULL) 
#' @param use_max (boolean) 
#' @param use_min (boolean)
#' @param verbatim_collectors (string) 
#' @param verbatim_datum (string) 
#' @param verbatim_date (string) 
#' @param verbatim_elevation (string) 
#' @param verbatim_geolocation_uncertainty (numeric) 
#' @param verbatim_habitat (string) 
#' @param verbatim_label (string)
#' @param verbatim_latitude (numeric) 
#' @param verbatim_locality (string) 
#' @param verbatim_longitude (numeric) 
#' @param verbatim_method (string) 
#' @param verbatim_trip_identifier (string) 
#' @param wkt (string) filter by well-known text describing the search area
#' @return list
#' @examples
#' \dontrun{
#' tw_collecting_events()
#' }
tw_collecting_events <- function( 
  citation_documents = NULL, citations = NULL, collection_objects = NULL,
  collector_id = NULL, collector_id_or = NULL, collectors = NULL,
  data_attribute_exact_value = NULL, data_attribute_predicate_id = NULL,
  data_attribute_value = NULL, data_attributes = NULL, document_label = NULL,
  elevation_precision = NULL, end_date = NULL, end_date_day = NULL,
  end_date_month = NULL, end_date_year = NULL, field_notes = NULL,
  formation = NULL, geographic_area = NULL, geographic_area_id = NULL,
  geographic_area_mode = NULL, georeferences = NULL, group = NULL,
  identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
  identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
  image_id = NULL, images = NULL, in_labels = NULL, in_verbatim_locality = NULL,
  keyword_id_and = NULL, keyword_id_or = NULL, lithology = NULL,
  local_identifiers = NULL, match_identifiers_delimiter = NULL,
  match_identifiers_type = NULL, maximum_elevation = NULL,
  md5_of_verbatim_label = NULL, md5_verbatim_label = NULL, member = NULL,
  meta_prioritize_geographic_area = NULL, min_ma = NULL,
  minimum_elevation = NULL, namespace_id = NULL, note_exact = NULL,
  note_text = NULL, notes = NULL, origin_citation = NULL, otu_id = NULL,
  print_label = NULL, radius = NULL, recent = NULL, start_date = NULL,
  start_date_day = NULL, start_date_month = NULL, start_date_year = NULL,
  tags = NULL, time_end_hour = NULL, time_end_minute = NULL,
  time_end_second = NULL, time_start_hour = NULL, time_start_minute = NULL,
  time_start_second = NULL, token = NULL, use_max = NULL, use_min = NULL,
  verbatim_collectors = NULL, verbatim_datum = NULL, verbatim_date = NULL,
  verbatim_elevation = NULL, verbatim_geolocation_uncertainty = NULL,
  verbatim_habitat = NULL, verbatim_label = NULL, verbatim_latitude = NULL,
  verbatim_locality = NULL, verbatim_longitude = NULL, verbatim_method = NULL,
  verbatim_trip_identifier = NULL, wkt = NULL, csv = FALSE, page = 0, per = 50, ...) {
  
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    citation_documents = citation_documents, citations = citations, collection_objects = collection_objects,
    collector_id = collector_id, collector_id_or = collector_id_or, collectors = collectors,
    data_attribute_exact_value = data_attribute_exact_value, data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_value = data_attribute_value, data_attributes = data_attributes, document_label = document_label,
    elevation_precision = elevation_precision, end_date = end_date, end_date_day = end_date_day,
    end_date_month = end_date_month, end_date_year = end_date_year, field_notes = field_notes,
    formation = formation, geographic_area = geographic_area, geographic_area_id = geographic_area_id,
    geographic_area_mode = geographic_area_mode, georeferences = georeferences, group = group,
    identifier = identifier, identifier_end = identifier_end, identifier_exact = identifier_exact,
    identifier_start = identifier_start, identifier_type = identifier_type, identifiers = identifiers,
    image_id = image_id, images = images, in_labels = in_labels, in_verbatim_locality = in_verbatim_locality,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or, lithology = lithology,
    local_identifiers = local_identifiers, match_identifiers_delimiter = match_identifiers_delimiter,
    match_identifiers_type = match_identifiers_type, maximum_elevation = maximum_elevation,
    md5_of_verbatim_label = md5_of_verbatim_label, md5_verbatim_label = md5_verbatim_label, member = member,
    meta_prioritize_geographic_area = meta_prioritize_geographic_area, min_ma = min_ma,
    minimum_elevation = minimum_elevation, namespace_id = namespace_id, note_exact = note_exact,
    note_text = note_text, notes = notes, origin_citation = origin_citation, otu_id = otu_id,
    print_label = print_label, radius = radius, recent = recent, start_date = start_date,
    start_date_day = start_date_day, start_date_month = start_date_month, start_date_year = start_date_year,
    tags = tags, time_end_hour = time_end_hour, time_end_minute = time_end_minute,
    time_end_second = time_end_second, time_start_hour = time_start_hour, time_start_minute = time_start_minute,
    time_start_second = time_start_second, token = token, use_max = use_max,
    use_min = use_min, verbatim_collectors = verbatim_collectors, verbatim_datum = verbatim_datum,
    verbatim_date = verbatim_date, verbatim_elevation = verbatim_elevation, verbatim_geolocation_uncertainty = verbatim_geolocation_uncertainty,
    verbatim_habitat = verbatim_habitat, verbatim_label = verbatim_label, verbatim_latitude = verbatim_latitude,
    verbatim_locality = verbatim_locality, verbatim_longitude = verbatim_longitude, verbatim_method = verbatim_method,
    verbatim_trip_identifier = verbatim_trip_identifier, wkt = wkt, page = page, per = per
  ))

  vector_params <- c("collector_id", "collector_id_or", "data_attribute_predicate_id",
    "data_attribute_value", "geographic_area_id", "identifier_type", "image_id", 
    "keyword_id_and", "keyword_id_or", "otu_id",
    "identifier_type", "taxon_name_id")

  df <- tw_GET(api_base_url(), "/collecting_events", query = args, csv = csv, vector_params = vector_params, ...)
  return(df)
}
#' Collection events
#' @rdname tw_collecting_events
#' @export
tw_ce <- tw_collecting_events


#' @return list
#' @examples
#' \dontrun{
#' tw_collecting_events()
#' }
tw_collecting_events <- function(
    csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {
  
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    token = token, project_token = project_token,
    page = page, per = per))

  df <- tw_GET(api_base_url(), "/collecting_events", query = args, csv = csv, ...)
  return(df)
}


#' Collection events
#' @rdname tw_collecting_events
#' @export
tw_ce <- tw_collecting_events
