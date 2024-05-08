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
#' @param citations (boolean) filiter by collection objects with/without citations
#' @param citation_documents (boolean) filter by collection objects with/without citation documents
#' @param collecting_event (string) filter by collection objects with/without collecting events
#' @param collecting_event_id (integer, vector) filter by collecting event id
#' @param collection_object_id (integer, vector) filter by collection object id
#' @param collection_object_type (string) filter by collection object type (Lot, RangedLot, Specimen)
#' @param collectors (boolean) filter by collection objects with/without collectors
#' @param current_determinations (boolean) filter by collection objects with/without current determinations
#' @param current_repository (boolean) filter by collection objects with/without current repository
#' @param current_repository_id (integer, vector) filter by current repository id
#' @param descendants (boolean) include collection objects for OTU descendants
#' @param dates (boolean) filter by collection objects with/without dates
#' @param determiner_id (integer, vector) filter by determiner id (People IDs)
#' @param determiner_id_or (boolean) use an or operator instead of an and operator for determiner_id
#' @param determiners (boolean) filter by collection objects with/without determiners
#' @param dwc_indexed (boolean) include metadata for Dwarin Core indexed collection objects
#' @param end_date (string) filter by end date
#' @param exact_buffered_collecting_event (string) filter by exact buffered collecting event text
#' @param exact_buffered_determinations (string) filter by exact buffered determinations text
#' @param exact_buffered_other_labels (string) filter by exact buffered other labels text
#' @param extract_id (integer, vector) filter by extract id
#' @param geojson (string) filter by geojson search area
#' @param geographic_area (boolean) filter by collection objects with/without geographic area
#' @param geographic_area_id (integer, vector) filter by geographic area id
#' @param georeferences (boolean) filter by collection objects with/without georeferences
#' @param image_id (integer, vector) filter by image id
#' @param identifier (string) filter by identifier associated with the collection object
#' @param identifier_end (string) filter by the last part of an identifier associated with the collection object
#' @param identifier_exact (string) filter by the exact identifier associated with the collection object
#' @param identifiers (boolean) filter by collection objects with/without identifiers
#' @param in_labels (string) wildcard search in all related collecting event labels
#' @param in_verbatim_locality (string) search in verbatim locality in related collecting events
#' @param is_type (string, vector) filter by types  # TODO: can it really be a vector?
#' @param keyword_id_and (string, vector) filter by keyword id with an and operator
#' @param keyword_id_or (string, vector) filter by keyword id with an or operator
#' @param loaned (boolean) filter by collection objects with/without loans
#' @param md5_verbatim_label (string) filter by md5 hash of verbatim label
#' @param namespace_id (integer) include metadata for associated identifier by namespace id
#' @param never_loaned (boolean) filter by collection objects that were never loaned
#' @param note_exact (string) search for the exact text in notes
#' @param notes (boolean) filter by collection objects with/without notes
#' @param on_loan (boolean) filter by collection objects currently on loan
#' @param origin_citation (boolean) filter by collection objects with/without origin citation
#' @param otu_id (integer, vector) filter by otu id
#' @param partial_overlap_dates (boolean) allow date overlaps
#' @param preparation_type (boolean) filter by collection objects with/without preparation type
#' @param preparation_type_id (integer, vector) filter by preparation type id
#' @param radius (integer) radius around location
#' @param repository (boolean) filter by collection objects with/without repository
#' @param repository_id (integer, vector) filter by repository id
#' @param sled_image_id (integer, vector) filter by sled image id
#' @param start_date (string) filter by start date
#' @param tags (boolean) filter by collection objects with/without tags
#' @param taxon_determination_id (integer, vector) filter by taxon determination id
#' @param taxon_determinations (boolean) filter by collection objects with/without taxon determinations
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param type_material (boolean) filter by collection objects with/without type material
#' @param type_specimen_taxon_name_id (integer) filter by taxon name id for type specimens
#' @param user_date_end (string) filter by user date end
#' @param user_date_start (string) filter by user date start
#' @param user_id (integer) filter by user id
#' @param user_target (string) target created or updated user date
#' @param validity (boolean) filter by collection objects with only valid ancestors
#' @param with_buffered_collecting_event (boolean) filter by collection objects with/without buffered collecting event
#' @param with_buffered_determinations (boolean) filter by collection objects with/without buffered determinations
#' @param with_buffered_other_labels (boolean) filter by collection objects with/without buffered other labels
#' @param wkt (string) filter by well-known text describing the search area
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_collection_objects()
#' }
tw_collection_objects <- function(biocuration_class_id = NULL, biological_association_id = NULL, 
  biological_associations = NULL, biological_relationship_id = NULL, buffered_collecting_event = NULL, 
  buffered_determinations = NULL, buffered_other_labels = NULL, citations = NULL, 
  citation_documents = NULL, collecting_event = NULL, collecting_event_id = NULL, 
  collection_object_id = NULL, collection_object_type = NULL, collectors = NULL, 
  current_determinations = NULL, current_repository = NULL, current_repository_id = NULL, 
  descendants = NULL, dates = NULL, determiner_id = NULL, determiner_id_or = NULL, 
  determiners = NULL, dwc_indexed = NULL, end_date = NULL, 
  exact_buffered_collecting_event = NULL, exact_buffered_determinations = NULL, 
  exact_buffered_other_labels = NULL, extract_id = NULL, geojson = NULL, 
  geographic_area = NULL, geographic_area_id = NULL, georeferences = NULL, 
  image_id = NULL, identifier = NULL, identifier_end = NULL, identifier_exact = NULL, 
  identifiers = NULL, in_labels = NULL, in_verbatim_locality = NULL, is_type = NULL, 
  keyword_id_and = NULL, keyword_id_or = NULL, loaned = NULL, md5_verbatim_label = NULL, 
  namespace_id = NULL, never_loaned = NULL, note_exact = NULL, notes = NULL, on_loan = NULL, 
  origin_citation = NULL, otu_id = NULL, partial_overlap_dates = NULL, preparation_type = NULL, 
  preparation_type_id = NULL, radius = NULL, repository = NULL, repository_id = NULL, 
  sled_image_id = NULL, start_date = NULL, tags = NULL, taxon_determination_id = NULL, 
  taxon_determinations = NULL, taxon_name_id = NULL, type_material = NULL, type_specimen_taxon_name_id = NULL, 
  user_date_end = NULL, user_date_start = NULL, user_id = NULL, user_target = NULL, validity = NULL, 
  with_buffered_collecting_event = NULL, with_buffered_determinations = NULL, with_buffered_other_labels = NULL, 
  wkt = NULL, csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {
  
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(biocuration_class_id = biocuration_class_id, biological_association_id = biological_association_id, 
    biological_associations = biological_associations, biological_relationship_id = biological_relationship_id, 
    buffered_collecting_event = buffered_collecting_event, buffered_determinations = buffered_determinations, 
    buffered_other_labels = buffered_other_labels, citations = citations, citation_documents = citation_documents, 
    collecting_event = collecting_event, collecting_event_id = collecting_event_id, 
    collection_object_id = collection_object_id, collection_object_type = collection_object_type, 
    collectors = collectors, current_determinations = current_determinations, current_repository = current_repository, 
    current_repository_id = current_repository_id, descendants = descendants, dates = dates, 
    determiner_id = determiner_id, determiner_id_or = determiner_id_or, determiners = determiners, 
    dwc_indexed = dwc_indexed, end_date = end_date, exact_buffered_collecting_event = exact_buffered_collecting_event, 
    exact_buffered_determinations = exact_buffered_determinations, exact_buffered_other_labels = exact_buffered_other_labels, 
    extract_id = extract_id, geojson = geojson, geographic_area = geographic_area, 
    geographic_area_id = geographic_area_id, georeferences = georeferences, image_id = image_id, 
    identifier = identifier, identifier_end = identifier_end, identifier_exact = identifier_exact, 
    identifiers = identifiers, in_labels = in_labels, in_verbatim_locality = in_verbatim_locality, 
    is_type = is_type, keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or, 
    loaned = loaned, md5_verbatim_label = md5_verbatim_label, namespace_id = namespace_id, 
    never_loaned = never_loaned, note_exact = note_exact, notes = notes, on_loan = on_loan, 
    origin_citation = origin_citation, otu_id = otu_id, partial_overlap_dates = partial_overlap_dates, 
    preparation_type = preparation_type, preparation_type_id = preparation_type_id, radius = radius, 
    repository = repository, repository_id = repository_id, sled_image_id = sled_image_id, 
    start_date = start_date, tags = tags, taxon_determination_id = taxon_determination_id, 
    taxon_determinations = taxon_determinations, taxon_name_id = taxon_name_id, type_material = type_material,
    type_specimen_taxon_name_id = type_specimen_taxon_name_id, user_date_end = user_date_end,
    user_date_start = user_date_start, user_id = user_id, user_target = user_target, validity = validity,
    with_buffered_collecting_event = with_buffered_collecting_event, with_buffered_determinations = with_buffered_determinations,
    with_buffered_other_labels = with_buffered_other_labels, wkt = wkt, token = token, project_token = project_token,
    page = page, per = per))

  vector_params <- c("biocuration_class_id", "biological_association_id", "biological_relationship_id", 
    "collecting_event_id", "collection_object_id", "current_repository_id", "determiner_id", "extract_id", 
    "geographic_area_id", "is_type", "image_id", "keyword_id_and", "keyword_id_or", "namespace_id", 
    "otu_id", "preparation_type_id", "repository_id", "sled_image_id", "taxon_determination_id", 
    "taxon_name_id", "type_specimen_taxon_name_id", "user_id")

  df <- tw_GET(api_base_url(), "/collection_objects", query = args, csv = csv, vector_params = vector_params, ...)
  return(df)
}


#' Collection objects
#' @rdname tw_collection_objects
#' @export
tw_co <- tw_collection_objects
