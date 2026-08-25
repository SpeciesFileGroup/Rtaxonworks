#' Collection objects
#'
#' @export
#' @param biocuration_class_id (integer, vector) filter by biocuration class id
#' @param biological_association_id (integer, vector) filter by biological association id
#' @param biological_associations (boolean) filter by collection objects with/without biological associations
#' @param biological_relationship_id (integer, vector) filter by biological relationship id
#' @param buffered_collecting_event (string) filter by buffered collecting event text
#' @param buffered_determinations (string) filter by buffered determinations text
#' @param buffered_other_labels (string) filter by buffered other labels text
#' @param collecting_event (string) filter by collection objects with/without collecting events
#' @param collecting_event_id (integer, vector) filter by collecting event id
#' @param collection_object_id (integer, vector) filter by collection object id
#' @param collection_object_type (string) filter by collection object type (`Lot`, `RangedLot`, `Specimen`)
#' @param collectors (boolean) filter by collection objects with/without collectors
#' @param container_id (integer, vector) filter by container id
#' @param containerized (boolean) filter by collection objects that are containerized
#' @param current_determinations (boolean) filter by collection objects with/without current determinations
#' @param current_repository (boolean) filter by collection objects with/without current repository
#' @param current_repository_id (integer, vector) filter by current repository id
#' @param dates (boolean) filter by collection objects with/without dates
#' @param deaccessioned (boolean) filter by deaccessioned collection objects
#' @param descendants (boolean) include collection objects for OTU descendants
#' @param determiner_id (integer, vector) filter by determiner id (People IDs)
#' @param determiner_id_all (boolean) require all listed determiner ids to match
#' @param determiners (boolean) filter by collection objects with/without determiners
#' @param dwc_indexed (boolean) include only Darwin Core indexed collection objects
#' @param exact_buffered_collecting_event (string) filter by exact buffered collecting event text
#' @param exact_buffered_determinations (string) filter by exact buffered determinations text
#' @param exact_buffered_other_labels (string) filter by exact buffered other labels text
#' @param extract_id (integer, vector) filter by extract id
#' @param import_dataset_id (integer, vector) filter by import dataset id
#' @param is_type (string, vector) filter by type category
#' @param loan_id (integer, vector) filter by loan id
#' @param loaned (boolean) filter by collection objects with/without loans
#' @param md5_verbatim_label (string) filter by md5 hash of verbatim label
#' @param never_loaned (boolean) filter by collection objects that were never loaned
#' @param on_loan (boolean) filter by collection objects currently on loan
#' @param otu_id (integer, vector) filter by otu id
#' @param preparation_type (boolean) filter by collection objects with/without preparation type
#' @param preparation_type_id (integer, vector) filter by preparation type id
#' @param repository (boolean) filter by collection objects with/without repository
#' @param repository_id (integer, vector) filter by repository id
#' @param sled_image_id (integer, vector) filter by sled image id
#' @param taxon_determinations (boolean) filter by collection objects with/without taxon determinations
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_current_determination (boolean) restrict taxon_name_id to current determinations
#' @param type_material (boolean) filter by collection objects with/without type material
#' @param type_specimen_taxon_name_id (integer) filter by taxon name id for type specimens
#' @param validity (boolean) filter by collection objects with only valid ancestors
#' @param with_buffered_collecting_event (boolean) filter by collection objects with/without buffered collecting event
#' @param with_buffered_determinations (boolean) filter by collection objects with/without buffered determinations
#' @param with_buffered_other_labels (boolean) filter by collection objects with/without buffered other labels
#' @param in_labels (string) wildcard search in all related collecting event labels
#' @param in_verbatim_locality (string) search in verbatim locality in related collecting events
#' @param georeferences (boolean) filter by collection objects with/without georeferences
#' @param geographic_area (boolean) filter by collection objects with/without geographic area
#' @param geographic_area_id (integer, vector) filter by geographic area id (legacy)
#' @param geo_collecting_event_geographic_area (boolean) include the geographic area of related collecting events
#' @param geo_json (string) GeoJSON geometry for spatial filtering
#' @param geo_mode (string) geographic matching mode
#' @param geo_shape_id (integer, vector) shape ids for geographic filtering
#' @param geo_shape_type (string, vector) shape types (`GeographicArea` or `Gazetteer`)
#' @param wkt (string) filter by well-known text describing the search area
#' @param wkt_geometry_type (string) restrict wkt filter to a geometry type
#' @param radius (integer) radius around location in meters
#' @param spatial_geographic_areas (boolean) filter by spatial geographic area presence
#' @param start_date (string) filter by start date
#' @param end_date (string) filter by end date
#' @param partial_overlap_dates (boolean) allow date overlaps
#' @param citations (boolean) filter by collection objects with/without citations
#' @param citation_documents (boolean) filter by collection objects with/without citation documents
#' @param origin_citation (boolean) filter by collection objects with/without origin citation
#' @param confidences (boolean) filter by collection objects with/without confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude collection objects with matching confidences
#' @param protocols (boolean) filter by collection objects with/without protocols
#' @param protocol_id_and (integer, vector) match all listed protocol ids
#' @param protocol_id_or (integer, vector) match any listed protocol ids
#' @param depictions (boolean) filter by collection objects with/without depictions
#' @param data_depictions (boolean) filter by collection objects with/without data depictions
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by collection objects with/without images
#' @param sound_id (integer, vector) filter by sound id (via conveyances)
#' @param sounds (boolean) filter by collection objects with/without sounds
#' @param conveyances (boolean) filter by collection objects with/without conveyances
#' @param data_attributes (boolean) filter by collection objects with/without data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string) filter by exact data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param any_value_attribute (string, vector) filter by any wildcard-matched attribute value
#' @param wildcard_attribute (string, vector) fields to search when using `any_value_attribute`
#' @param identifier (string) filter by identifier associated with the collection object
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type
#' @param identifiers (boolean) filter by collection objects with/without identifiers
#' @param global_identifiers (boolean) filter by collection objects with/without global identifiers
#' @param local_identifiers (boolean) filter by collection objects with/without local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string of identifiers
#' @param match_identifiers_delimiter (string) delimiter for match_identifiers
#' @param match_identifiers_type (string) internal or identifier
#' @param namespace_id (integer) filter by namespace id
#' @param note_exact (boolean) require exact match on note text
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by collection objects with/without notes
#' @param keyword_id_and (integer, vector) filter by keyword id with an and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with an or operator
#' @param tags (boolean) filter by collection objects with/without tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @param user_id (integer) filter by user id
#' @param user_target (string) target `created` or `updated`
#' @param user_date_start (string) filter by user date start (ISO 8601)
#' @param user_date_end (string) filter by user date end (ISO 8601)
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_collection_objects()
#' }
tw_collection_objects <- function(biocuration_class_id = NULL,
  biological_association_id = NULL, biological_associations = NULL,
  biological_relationship_id = NULL, buffered_collecting_event = NULL,
  buffered_determinations = NULL, buffered_other_labels = NULL,
  collecting_event = NULL, collecting_event_id = NULL, collection_object_id = NULL,
  collection_object_type = NULL, collectors = NULL, container_id = NULL,
  containerized = NULL, current_determinations = NULL, current_repository = NULL,
  current_repository_id = NULL, dates = NULL, deaccessioned = NULL,
  descendants = NULL, determiner_id = NULL, determiner_id_all = NULL,
  determiners = NULL, dwc_indexed = NULL,
  exact_buffered_collecting_event = NULL, exact_buffered_determinations = NULL,
  exact_buffered_other_labels = NULL, extract_id = NULL, import_dataset_id = NULL,
  is_type = NULL, loan_id = NULL, loaned = NULL, md5_verbatim_label = NULL,
  never_loaned = NULL, on_loan = NULL, otu_id = NULL,
  preparation_type = NULL, preparation_type_id = NULL, repository = NULL,
  repository_id = NULL, sled_image_id = NULL,
  taxon_determinations = NULL, taxon_name_id = NULL,
  taxon_name_current_determination = NULL, type_material = NULL,
  type_specimen_taxon_name_id = NULL, validity = NULL,
  with_buffered_collecting_event = NULL, with_buffered_determinations = NULL,
  with_buffered_other_labels = NULL,
  in_labels = NULL, in_verbatim_locality = NULL, georeferences = NULL,
  geographic_area = NULL, geographic_area_id = NULL,
  geo_collecting_event_geographic_area = NULL, geo_json = NULL, geo_mode = NULL,
  geo_shape_id = NULL, geo_shape_type = NULL, wkt = NULL,
  wkt_geometry_type = NULL, radius = NULL, spatial_geographic_areas = NULL,
  start_date = NULL, end_date = NULL, partial_overlap_dates = NULL,
  citations = NULL, citation_documents = NULL, origin_citation = NULL,
  confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
  protocols = NULL, protocol_id_and = NULL, protocol_id_or = NULL,
  depictions = NULL, data_depictions = NULL, image_id = NULL, images = NULL,
  sound_id = NULL, sounds = NULL, conveyances = NULL,
  data_attributes = NULL, data_attribute_predicate_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_value = NULL,
  any_value_attribute = NULL, wildcard_attribute = NULL,
  identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
  identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
  global_identifiers = NULL, local_identifiers = NULL, match_identifiers = NULL,
  match_identifiers_delimiter = NULL, match_identifiers_type = NULL,
  namespace_id = NULL, note_exact = NULL, note_text = NULL, notes = NULL,
  keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, exclude_tags = NULL,
  user_id = NULL, user_target = NULL, user_date_start = NULL, user_date_end = NULL,
  csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    biocuration_class_id = biocuration_class_id,
    biological_association_id = biological_association_id,
    biological_associations = biological_associations,
    biological_relationship_id = biological_relationship_id,
    buffered_collecting_event = buffered_collecting_event,
    buffered_determinations = buffered_determinations,
    buffered_other_labels = buffered_other_labels,
    collecting_event = collecting_event, collecting_event_id = collecting_event_id,
    collection_object_id = collection_object_id,
    collection_object_type = collection_object_type, collectors = collectors,
    container_id = container_id, containerized = containerized,
    current_determinations = current_determinations,
    current_repository = current_repository,
    current_repository_id = current_repository_id, dates = dates,
    deaccessioned = deaccessioned, descendants = descendants,
    determiner_id = determiner_id, determiner_id_all = determiner_id_all,
    determiners = determiners, dwc_indexed = dwc_indexed,
    exact_buffered_collecting_event = exact_buffered_collecting_event,
    exact_buffered_determinations = exact_buffered_determinations,
    exact_buffered_other_labels = exact_buffered_other_labels,
    extract_id = extract_id, import_dataset_id = import_dataset_id,
    is_type = is_type, loan_id = loan_id, loaned = loaned,
    md5_verbatim_label = md5_verbatim_label, never_loaned = never_loaned,
    on_loan = on_loan, otu_id = otu_id,
    preparation_type = preparation_type,
    preparation_type_id = preparation_type_id, repository = repository,
    repository_id = repository_id, sled_image_id = sled_image_id,
    taxon_determinations = taxon_determinations, taxon_name_id = taxon_name_id,
    taxon_name_current_determination = taxon_name_current_determination,
    type_material = type_material,
    type_specimen_taxon_name_id = type_specimen_taxon_name_id,
    validity = validity,
    with_buffered_collecting_event = with_buffered_collecting_event,
    with_buffered_determinations = with_buffered_determinations,
    with_buffered_other_labels = with_buffered_other_labels,
    in_labels = in_labels, in_verbatim_locality = in_verbatim_locality,
    georeferences = georeferences, geographic_area = geographic_area,
    geographic_area_id = geographic_area_id,
    geo_collecting_event_geographic_area = geo_collecting_event_geographic_area,
    geo_json = geo_json, geo_mode = geo_mode,
    geo_shape_id = geo_shape_id, geo_shape_type = geo_shape_type,
    wkt = wkt, wkt_geometry_type = wkt_geometry_type, radius = radius,
    spatial_geographic_areas = spatial_geographic_areas,
    start_date = start_date, end_date = end_date,
    partial_overlap_dates = partial_overlap_dates,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    protocols = protocols, protocol_id_and = protocol_id_and,
    protocol_id_or = protocol_id_or,
    depictions = depictions, data_depictions = data_depictions,
    image_id = image_id, images = images,
    sound_id = sound_id, sounds = sounds, conveyances = conveyances,
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
    user_id = user_id, user_target = user_target,
    user_date_start = user_date_start, user_date_end = user_date_end,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("any_value_attribute", "biocuration_class_id",
    "biological_association_id", "biological_relationship_id",
    "collecting_event_id", "collection_object_id", "confidence_level_id",
    "container_id", "current_repository_id", "data_attribute_predicate_id",
    "data_attribute_value", "determiner_id", "extract_id",
    "geo_shape_id", "geo_shape_type", "geographic_area_id",
    "identifier_type", "image_id", "import_dataset_id", "is_type",
    "keyword_id_and", "keyword_id_or", "loan_id", "otu_id",
    "preparation_type_id", "protocol_id_and", "protocol_id_or",
    "repository_id", "sled_image_id", "sound_id", "taxon_name_id",
    "wildcard_attribute")

  df <- tw_GET(api_base_url(), "/collection_objects", query = args, csv = csv, vector_params = vector_params, ...)
  return(df)
}


#' Collection objects
#' @rdname tw_collection_objects
#' @export
tw_co <- tw_collection_objects
