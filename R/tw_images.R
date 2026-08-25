#' Images
#'
#' @export
#' @template args
#' @param biocuration_class_id (integer, vector) filter by biocuration class id
#' @param collection_object_id (integer, vector) filter by collection object id
#' @param collection_object_scope (string, vector) collection object scope (`all`, `collection_objects`, `observations`, `collecting_events`)
#' @param field_occurrence_id (integer, vector) filter by field occurrence id
#' @param field_occurrence_scope (string, vector) field occurrence scope
#' @param image_id (integer, vector) filter by image id
#' @param otu_id (integer, vector) filter by otu id
#' @param otu_scope (string, vector) otu scope
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_id_target (string) taxon name id match target
#' @param source_id (integer, vector) filter by source id
#' @param depictions (boolean) filter by images that are used in a depiction
#' @param depiction_object_type (string, vector) filter by depiction object type
#' @param depiction_caption (string) filter by depiction caption
#' @param depiction_caption_exact (boolean) require exact match on depiction caption
#' @param metadata_depiction (boolean) filter by images that are metadata depictions
#' @param type_material_depictions (boolean) filter by images used in type material depictions
#' @param freeform_svg (boolean) filter by images with freeform SVG annotations
#' @param sled_image (boolean) filter by sled images
#' @param sled_image_id (integer, vector) filter by sled image id
#' @param sqed_image (boolean) filter by sqed images
#' @param creator_id (integer, vector) filter by image creator (person) id
#' @param creator_id_all (boolean) require all listed creator ids to match
#' @param editor_id (integer, vector) filter by image editor (person) id
#' @param editor_id_all (boolean) require all listed editor ids to match
#' @param owner_id (integer, vector) filter by image owner (person) id
#' @param owner_id_all (boolean) require all listed owner ids to match
#' @param owner_organization_id (integer, vector) filter by owner organization id
#' @param copyright_holder_id (integer, vector) filter by copyright holder (person) id
#' @param copyright_holder_id_all (boolean) require all listed copyright holder ids to match
#' @param copyright_holder_organization_id (integer, vector) filter by copyright holder organization id
#' @param copyright_year (integer) filter by exact copyright year
#' @param copyright_after_year (integer) filter by copyright year after
#' @param copyright_prior_to_year (integer) filter by copyright year before
#' @param license (string, vector) filter by license
#' @param citations (boolean) filter by images with citations
#' @param citation_documents (boolean) filter by images with citation documents
#' @param origin_citation (boolean) filter by images with origin citations
#' @param note_text (string) filter by note text
#' @param note_exact (boolean) require exact match on note text
#' @param notes (boolean) filter by images with notes
#' @param keyword_id_and (integer, vector) filter by keyword id (AND)
#' @param keyword_id_or (integer, vector) filter by keyword id (OR)
#' @param tags (boolean) filter by images with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type
#' @param identifiers (boolean) filter by images with identifiers
#' @param global_identifiers (boolean) filter by images with global identifiers
#' @param local_identifiers (boolean) filter by images with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string
#' @param match_identifiers_delimiter (string) delimiter for match_identifiers
#' @param match_identifiers_type (string) internal or identifier
#' @param namespace_id (integer) filter by namespace id
#' @return list
#' @examples
#' \dontrun{
#' tw_images(image_id = c(3739, 4178, 4179))
#' }
tw_images <- function(
    biocuration_class_id = NULL, collection_object_id = NULL,
    collection_object_scope = NULL, field_occurrence_id = NULL,
    field_occurrence_scope = NULL, image_id = NULL, otu_id = NULL,
    otu_scope = NULL, taxon_name_id = NULL, taxon_name_id_target = NULL,
    source_id = NULL,
    depictions = NULL, depiction_object_type = NULL, depiction_caption = NULL,
    depiction_caption_exact = NULL, metadata_depiction = NULL,
    type_material_depictions = NULL, freeform_svg = NULL,
    sled_image = NULL, sled_image_id = NULL, sqed_image = NULL,
    creator_id = NULL, creator_id_all = NULL, editor_id = NULL,
    editor_id_all = NULL, owner_id = NULL, owner_id_all = NULL,
    owner_organization_id = NULL, copyright_holder_id = NULL,
    copyright_holder_id_all = NULL, copyright_holder_organization_id = NULL,
    copyright_year = NULL, copyright_after_year = NULL,
    copyright_prior_to_year = NULL, license = NULL,
    citations = NULL, citation_documents = NULL, origin_citation = NULL,
    note_text = NULL, note_exact = NULL, notes = NULL,
    keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL, exclude_tags = NULL,
    identifier = NULL, identifier_end = NULL, identifier_exact = NULL,
    identifier_start = NULL, identifier_type = NULL, identifiers = NULL,
    global_identifiers = NULL, local_identifiers = NULL, match_identifiers = NULL,
    match_identifiers_delimiter = NULL, match_identifiers_type = NULL,
    namespace_id = NULL,
    csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    biocuration_class_id = biocuration_class_id,
    collection_object_id = collection_object_id,
    collection_object_scope = collection_object_scope,
    field_occurrence_id = field_occurrence_id,
    field_occurrence_scope = field_occurrence_scope,
    image_id = image_id, otu_id = otu_id, otu_scope = otu_scope,
    taxon_name_id = taxon_name_id,
    taxon_name_id_target = taxon_name_id_target, source_id = source_id,
    depictions = depictions, depiction_object_type = depiction_object_type,
    depiction_caption = depiction_caption,
    depiction_caption_exact = depiction_caption_exact,
    metadata_depiction = metadata_depiction,
    type_material_depictions = type_material_depictions,
    freeform_svg = freeform_svg,
    sled_image = sled_image, sled_image_id = sled_image_id,
    sqed_image = sqed_image,
    creator_id = creator_id, creator_id_all = creator_id_all,
    editor_id = editor_id, editor_id_all = editor_id_all,
    owner_id = owner_id, owner_id_all = owner_id_all,
    owner_organization_id = owner_organization_id,
    copyright_holder_id = copyright_holder_id,
    copyright_holder_id_all = copyright_holder_id_all,
    copyright_holder_organization_id = copyright_holder_organization_id,
    copyright_year = copyright_year,
    copyright_after_year = copyright_after_year,
    copyright_prior_to_year = copyright_prior_to_year, license = license,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation,
    note_text = note_text, note_exact = note_exact, notes = notes,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    tags = tags, exclude_tags = exclude_tags,
    identifier = identifier, identifier_end = identifier_end,
    identifier_exact = identifier_exact, identifier_start = identifier_start,
    identifier_type = identifier_type, identifiers = identifiers,
    global_identifiers = global_identifiers, local_identifiers = local_identifiers,
    match_identifiers = match_identifiers,
    match_identifiers_delimiter = match_identifiers_delimiter,
    match_identifiers_type = match_identifiers_type,
    namespace_id = namespace_id,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("biocuration_class_id", "collection_object_id",
    "collection_object_scope", "copyright_holder_id",
    "copyright_holder_organization_id", "creator_id", "depiction_object_type",
    "editor_id", "field_occurrence_id", "field_occurrence_scope",
    "identifier_type", "image_id", "keyword_id_and", "keyword_id_or",
    "license", "otu_id", "otu_scope", "owner_id", "owner_organization_id",
    "sled_image_id", "source_id", "taxon_name_id")

  res <- tw_GET(api_base_url(), "/images", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
