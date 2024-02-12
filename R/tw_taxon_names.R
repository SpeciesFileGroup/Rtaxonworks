#' Taxon Names
#'
#' @export
#' @param ancestors (boolean) include ancestors
#' @param author (string) filter by cached_author_year
#' @param author_exact (boolean) filter by exact match on cached_author_year
#' @param authors (boolean) filter by taxon names that have an author string present
#' @param citations (boolean) filter by taxon names with citations
#' @param citation_documents (boolean) filter by taxon names with citation documents
#' @param collecting_event_id (integer) filter by collecting event id
#' @param collection_object_id (integer) filter by collection object id
#' @param combination_taxon_name_id (integer) filter by combination taxon name id
#' @param data_attribute_exact_value (boolean) filter by exact match on data attribute value
#' @param data_attribute_predicate_id (integer) filter by data attribute predicate id
#' @param data_attribute_value (string) filter by data attribute value
#' @param data_attributes (boolean) filter by taxon names that have 1 or more data attributes
#' @param descendants (boolean) include descendants
#' @param descendants_max_depth (integer) maximum depth of descendants to include
#' @param etymology (boolean) filter by taxon names with etymology
#' @param exact (boolean) filter by exact match on name
#' @param image_id (integer) filter by taxon names associated with an image id
#' @param images (boolean) filter by taxon names with images
#' @param keyword_id_and (integer) filter by keyword id with and operator
#' @param keyword_id_or (integer) filter by keyword id with or operator
#' @param leaves (boolean) filiter by names having no descendants
#' @param name (string) filter by name
#' @param name_exact (boolean) filter by exact match on name
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by taxon names with notes
#' @param nomenclatural_code (string) filter by nomenclatural code
#' @param nomenclature_group (string) filter by nomenclature group
#' @param not_specified (boolean) filter by whether the name has NOT SPECIFIED in one of the cached values
#' @param origin_citation (boolean) filter by taxon names with an origin citation
#' @param original_combination (boolean) filter by names with at least one element of the original combination
#' @param otus (boolean) filter by names that have one or more Otus
#' @param parent_id (integer) filter by parent id
#' @param rank (string) filter by rank class (e.g., NomenclaturalRank::Iczn::SpeciesGroup::Species)
#' @param tags (boolean) filter by taxon names with tags
#' @param taxon_name_author_id_or (boolean)
#' @param taxon_name_id (integer) filter by taxon name id
#' @param taxon_name_relationship (string) filter by taxon name relationship
#' @param taxon_name_relationship_type (string) filter by taxon name relationship type (e.g., TaxonNameRelationship::Iczn::Invalidating)
#' @param taxon_name_type (string) filter by taxon name type (e.g., Protonym, Combination, Hybrid)
#' @param type_metadata (boolean) filter by taxon names that are linked to type specimen material
#' @param validity (boolean) filter by taxon names that are valid
#' @param year (integer) filter by authority year in the cached_author_year field
#' @param year_end (integer) filter by taxon names with the cached_author_year field less than or equal to this value
#' @param year_start (integer) filter by taxon names with the cached_author_year field greater than or equal to this value
#' @template args
#' @return list
#' @examples
#' tw_taxon_names(name="Lycorma delicatula", valid=TRUE)
tw_taxon_names <- function(ancestors = NULL, author = NULL, 
  author_exact = NULL, authors = NULL, citations = NULL,
  citation_documents = NULL, collecting_event_id = NULL,
  collection_object_id = NULL, combination_taxon_name_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_predicate_id = NULL,
  data_attribute_value = NULL, data_attributes = NULL, descendants = NULL,
  descendants_max_depth = NULL, etymology = NULL, exact = NULL,
  image_id = NULL, images = NULL, keyword_id_and = NULL,
  keyword_id_or = NULL, leaves = NULL, name = NULL, name_exact = NULL,
  note_exact = NULL, note_text = NULL, notes = NULL, nomenclatural_code = NULL,
  nomenclature_group = NULL, not_specified = NULL, origin_citation = NULL,
  original_combination = NULL, otus = NULL, parent_id = NULL, rank = NULL,
  tags = NULL, taxon_name_author_id_or = NULL, taxon_name_id = NULL,
  taxon_name_relationship = NULL, taxon_name_relationship_type = NULL,
  taxon_name_type = NULL, type_metadata = NULL, validity = NULL, year = NULL,
  year_end = NULL, year_start = NULL, page = 0, per = 50, ...) {
  
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(ancestors = ancestors, author = author, 
    author_exact = author_exact, authors = authors, citations = citations,
    citation_documents = citation_documents, 
    collecting_event_id = collecting_event_id,
    collection_object_id = collection_object_id, 
    combination_taxon_name_id = combination_taxon_name_id,
    data_attribute_exact_value = data_attribute_exact_value, 
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_value = data_attribute_value, data_attributes = data_attributes, 
    descendants = descendants, descendants_max_depth = descendants_max_depth, 
    etymology = etymology, exact = exact, image_id = image_id, images = images, 
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or, leaves = leaves, 
    name = name, name_exact = name_exact, note_exact = note_exact, note_text = note_text, 
    notes = notes, nomenclatural_code = nomenclatural_code, 
    nomenclature_group = nomenclature_group, not_specified = not_specified, 
    origin_citation = origin_citation, original_combination = original_combination, 
    otus = otus, parent_id = parent_id, rank = rank, tags = tags, 
    taxon_name_author_id_or = taxon_name_author_id_or, taxon_name_id = taxon_name_id,
    taxon_name_relationship = taxon_name_relationship, 
    taxon_name_relationship_type = taxon_name_relationship_type,
    taxon_name_type = taxon_name_type, type_metadata = type_metadata, validity = validity, 
    year = year, year_end = year_end, year_start = year_start, page = page, per = per))

  tmp <- tw_GET(api_base_url(), "/taxon_names", body = args, ...)
  tmp <- tibble::as_tibble(tmp)
  return(tmp)
}
