#' Taxon Names
#'
#' @export
#' @param ancestors (boolean) include ancestors (requires taxon_name_id)
#' @param ancestrify (boolean) extend result to include ancestors of names in the result
#' @param author (string) filter by cached_author_year
#' @param author_exact (boolean) filter by exact match on cached_author_year
#' @param authors (boolean) filter by taxon names that have an author string
#' @param availability (boolean) filter by availability
#' @param cached (string, vector) filter by cached string
#' @param citations (boolean) filter by taxon names with citations
#' @param citation_documents (boolean) filter by taxon names with citation documents
#' @param origin_citation (boolean) filter by taxon names with an origin citation
#' @param collecting_event_id (integer, vector) filter by collecting event id
#' @param collection_object_id (integer, vector) filter by collection object id
#' @param combinations (boolean) include combinations in the result set
#' @param combinationify (boolean) extend result to include combinations
#' @param combination_taxon_name_id (integer, vector) filter by combination taxon name id
#' @param confidences (boolean) filter by taxon names with confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param exclude_confidences (boolean) exclude taxon names with matching confidences
#' @param data_attributes (boolean) filter by taxon names with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (string) filter by exact data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param depictions (boolean) filter by taxon names with depictions
#' @param data_depictions (boolean) filter by taxon names with data depictions
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by taxon names with images
#' @param descendants (boolean) include descendants (requires taxon_name_id)
#' @param descendants_max_depth (integer) maximum depth of descendants to include
#' @param epithet_only (boolean) match on `name` property instead of `cached`
#' @param etymology (boolean) filter by taxon names with etymology
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param tags (boolean) filter by taxon names with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @param latinized (boolean) filter by latinized names
#' @param leaves (boolean) filter to names with no descendants
#' @param name (string, vector) filter by name
#' @param name_exact (boolean) filter by exact match on name (paired with `name`)
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by taxon names with notes
#' @param nomenclature_code (string) filter by nomenclatural code
#' @param nomenclature_date (string) filter by nomenclature date
#' @param nomenclature_group (string) filter by nomenclature group
#' @param not_specified (boolean) filter by names with NOT SPECIFIED in a cached value
#' @param original_combination (boolean) filter by names in an original combination
#' @param otu_id (integer, vector) filter by otu id
#' @param otus (boolean) filter by names with one or more OTUs
#' @param parent_id (integer, vector) filter by parent id
#' @param rank (string, vector) filter by rank class (e.g., `NomenclaturalRank::Iczn::SpeciesGroup::Species`)
#' @param relation_to_relationship (string) relation-to-relationship modifier
#' @param sort (string) sort order
#' @param synonymify (boolean) extend result to include synonyms
#' @param taxon_name_author_id (integer, vector) filter by author (Person) id
#' @param taxon_name_author_id_or (boolean) OR-mode for taxon_name_author_id
#' @param taxon_name_classification (string, vector) filter by taxon name classification
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_relationship (list) nested filter: list of lists with `subject_taxon_name_id`, `object_taxon_name_id`, and `type`
#' @param taxon_name_relationship_target (string) relationship target
#' @param taxon_name_relationship_type_subject (string, vector) filter by relationship type on the subject side
#' @param taxon_name_relationship_type_object (string, vector) filter by relationship type on the object side
#' @param taxon_name_relationship_type_either (string, vector) filter by relationship type on either side
#' @param taxon_name_type (string) filter by taxon name type (e.g., `Protonym`, `Combination`, `Hybrid`)
#' @param type (string, vector) filter by STI type
#' @param type_metadata (boolean) filter by taxon names linked to type specimen material
#' @param validify (boolean) extend result to include valid names
#' @param validity (boolean) filter by valid names
#' @param verbatim_author (string) filter by verbatim author
#' @param verbatim_name (string) filter by verbatim name
#' @param year (integer) filter by authority year in cached_author_year
#' @param year_end (integer) filter by cached_author_year <= this value
#' @param year_start (integer) filter by cached_author_year >= this value
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type
#' @param identifiers (boolean) filter by taxon names with identifiers
#' @param global_identifiers (boolean) filter by taxon names with global identifiers
#' @param local_identifiers (boolean) filter by taxon names with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string
#' @param match_identifiers_delimiter (string) delimiter for `match_identifiers`
#' @param match_identifiers_type (string) internal or identifier
#' @param namespace_id (integer) filter by namespace id
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_taxon_names(name = "Lycorma delicatula", validity = TRUE)
#' }
tw_taxon_names <- function(ancestors = NULL, ancestrify = NULL, author = NULL,
  author_exact = NULL, authors = NULL, availability = NULL, cached = NULL,
  citations = NULL, citation_documents = NULL, origin_citation = NULL,
  collecting_event_id = NULL, collection_object_id = NULL,
  combinations = NULL, combinationify = NULL, combination_taxon_name_id = NULL,
  confidences = NULL, confidence_level_id = NULL, exclude_confidences = NULL,
  data_attributes = NULL, data_attribute_predicate_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_value = NULL,
  depictions = NULL, data_depictions = NULL, image_id = NULL, images = NULL,
  descendants = NULL, descendants_max_depth = NULL, epithet_only = NULL,
  etymology = NULL, keyword_id_and = NULL, keyword_id_or = NULL,
  tags = NULL, exclude_tags = NULL, latinized = NULL, leaves = NULL,
  name = NULL, name_exact = NULL,
  note_exact = NULL, note_text = NULL, notes = NULL,
  nomenclature_code = NULL, nomenclature_date = NULL, nomenclature_group = NULL,
  not_specified = NULL, original_combination = NULL,
  otu_id = NULL, otus = NULL, parent_id = NULL, rank = NULL,
  relation_to_relationship = NULL, sort = NULL, synonymify = NULL,
  taxon_name_author_id = NULL, taxon_name_author_id_or = NULL,
  taxon_name_classification = NULL, taxon_name_id = NULL,
  taxon_name_relationship = NULL, taxon_name_relationship_target = NULL,
  taxon_name_relationship_type_subject = NULL,
  taxon_name_relationship_type_object = NULL,
  taxon_name_relationship_type_either = NULL,
  taxon_name_type = NULL, type = NULL, type_metadata = NULL,
  validify = NULL, validity = NULL, verbatim_author = NULL, verbatim_name = NULL,
  year = NULL, year_end = NULL, year_start = NULL,
  identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
  identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
  global_identifiers = NULL, local_identifiers = NULL, match_identifiers = NULL,
  match_identifiers_delimiter = NULL, match_identifiers_type = NULL,
  namespace_id = NULL,
  csv = TRUE, token = NULL, project_token = NULL,
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(ancestors = ancestors, ancestrify = ancestrify, author = author,
    author_exact = author_exact, authors = authors, availability = availability,
    cached = cached, citations = citations,
    citation_documents = citation_documents, origin_citation = origin_citation,
    collecting_event_id = collecting_event_id,
    collection_object_id = collection_object_id,
    combinations = combinations, combinationify = combinationify,
    combination_taxon_name_id = combination_taxon_name_id,
    confidences = confidences, confidence_level_id = confidence_level_id,
    exclude_confidences = exclude_confidences,
    data_attributes = data_attributes,
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_value = data_attribute_value,
    depictions = depictions, data_depictions = data_depictions,
    image_id = image_id, images = images,
    descendants = descendants, descendants_max_depth = descendants_max_depth,
    epithet_only = epithet_only, etymology = etymology,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    tags = tags, exclude_tags = exclude_tags,
    latinized = latinized, leaves = leaves,
    name = name, name_exact = name_exact,
    note_exact = note_exact, note_text = note_text, notes = notes,
    nomenclature_code = nomenclature_code,
    nomenclature_date = nomenclature_date,
    nomenclature_group = nomenclature_group,
    not_specified = not_specified,
    original_combination = original_combination,
    otu_id = otu_id, otus = otus, parent_id = parent_id, rank = rank,
    relation_to_relationship = relation_to_relationship,
    sort = sort, synonymify = synonymify,
    taxon_name_author_id = taxon_name_author_id,
    taxon_name_author_id_or = taxon_name_author_id_or,
    taxon_name_classification = taxon_name_classification,
    taxon_name_id = taxon_name_id,
    taxon_name_relationship = taxon_name_relationship,
    taxon_name_relationship_target = taxon_name_relationship_target,
    taxon_name_relationship_type_subject = taxon_name_relationship_type_subject,
    taxon_name_relationship_type_object = taxon_name_relationship_type_object,
    taxon_name_relationship_type_either = taxon_name_relationship_type_either,
    taxon_name_type = taxon_name_type, type = type,
    type_metadata = type_metadata, validify = validify, validity = validity,
    verbatim_author = verbatim_author, verbatim_name = verbatim_name,
    year = year, year_end = year_end, year_start = year_start,
    identifier = identifier, identifier_end = identifier_end,
    identifier_exact = identifier_exact, identifier_start = identifier_start,
    identifier_type = identifier_type, identifiers = identifiers,
    global_identifiers = global_identifiers, local_identifiers = local_identifiers,
    match_identifiers = match_identifiers,
    match_identifiers_delimiter = match_identifiers_delimiter,
    match_identifiers_type = match_identifiers_type,
    namespace_id = namespace_id,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("cached", "collecting_event_id", "collection_object_id",
    "combination_taxon_name_id", "confidence_level_id",
    "data_attribute_predicate_id", "data_attribute_value", "identifier_type",
    "image_id", "keyword_id_and", "keyword_id_or", "name", "otu_id", "parent_id",
    "rank", "taxon_name_author_id", "taxon_name_classification",
    "taxon_name_id", "taxon_name_relationship_type_either",
    "taxon_name_relationship_type_object",
    "taxon_name_relationship_type_subject", "type")

  res <- tw_GET(api_base_url(), "/taxon_names", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Taxon Names
#'
#' @rdname tw_taxon_names
#' @export
tw_tn <- tw_taxon_names


#' Taxon Names Autocomplete
#'
#' @export
#' @template args
#' @param term (string) filter by search term
#' @param exact (boolean) filter by exact match on name
#' @param no_leaves (boolean) filter by names having no descendants
#' @param nomenclature_group (string, vector) filter by nomenclature group (e.g., `Family`, `Genus`, `Species`)
#' @param parent_id (integer, vector) filter by parent id
#' @param type (string, vector) filter by type (e.g., `Protonym`, `Combination`)
#' @param valid (boolean) filter by valid or invalid names
#' @return list
#' @examples
#' \dontrun{
#' tw_taxon_names_autocomplete(term = "Lycorma")
#' }
tw_taxon_names_autocomplete <- function(term = NULL, exact = NULL, no_leaves = NULL,
  nomenclature_group = NULL, parent_id = NULL, type = NULL,
  valid = NULL, csv = FALSE, token = NULL, project_token = NULL,
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(term = term, exact = exact, no_leaves = no_leaves,
    nomenclature_group = nomenclature_group, parent_id = parent_id,
    type = type, valid = valid,
    token = token, project_token = project_token, page = page, per = per))

  vector_params = c("nomenclature_group", "parent_id", "type")

  res <- tw_GET(api_base_url(), "/taxon_names/autocomplete", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Taxon Names Autocomplete
#'
#' @rdname tw_taxon_names_autocomplete
#' @export
tw_tn_auto <- tw_taxon_names_autocomplete
