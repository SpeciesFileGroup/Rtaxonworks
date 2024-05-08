#' Otus
#'
#' @export
#' @param id (integer) access an OTU by ID
#' @param subresource (string) access an API subresource (e.g., inventory/dwc)
#' @param asserted_distribution_ids (string) filter by asserted distribution ids
#' @param biological_association_ids (string) filter by biological association ids
#' @param citations (boolean) filter by otus with citations
#' @param citations_documents (boolean) filter by otus with citation documents
#' @param data_attribute_exact_value (boolean) require exact match on data attribute value
#' @param data_attribute_predicate_id (integer) filter by data attribute predicate id
#' @param data_attribute_value (string) filter by data attribute value
#' @param data_attributes (boolean) filter by otus with data attributes
#' @param data_attributes_attributes (string) filter by data attributes attributes
#' @param image_id (integer) filter by image id
#' @param images (boolean) filter by otus with images
#' @param keyword_id_and (integer) filter by keyword id with and operator
#' @param keyword_id_or (integer) filter by keyword id with or operator
#' @param name (string) filter by name
#' @param note_exact (boolean) require exact match on note text
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by otus with notes
#' @param origin_citations (boolean) filter by otus with origin citations
#' @param otu_id (integer) filter by otu id
#' @param tags (boolean) filter by otus with tags
#' @param taxon_name_classification_id (string) filter by taxon name classification ids
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_relationship_id (string, vector) filter by taxon name relationship ids
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_otus()
#' }
tw_otus <- function(id = NULL, subresource = NULL, asserted_distribution_ids = NULL, 
  biological_association_ids = NULL,
  citations = NULL, citations_documents = NULL, data_attribute_exact_value = NULL,
  data_attribute_predicate_id = NULL, data_attribute_value = NULL, data_attributes = NULL,
  data_attributes_attributes = NULL, image_id = NULL, images = NULL, keyword_id_and = NULL,
  keyword_id_or = NULL, name = NULL, note_exact = NULL, note_text = NULL, notes = NULL,
  origin_citations = NULL, otu_id = NULL, tags = NULL,
  taxon_name_classification_id = NULL, taxon_name_id = NULL, 
  taxon_name_relationship_id = NULL, csv = FALSE, token = NULL, project_token = NULL, 
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(asserted_distribution_ids = asserted_distribution_ids,
    biological_association_ids = biological_association_ids, citations = citations,
    citations_documents = citations_documents, data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_predicate_id = data_attribute_predicate_id, data_attribute_value = data_attribute_value,
    data_attributes = data_attributes, data_attributes_attributes = data_attributes_attributes,
    image_id = image_id, images = images, keyword_id_and = keyword_id_and,
    keyword_id_or = keyword_id_or, name = name, note_exact = note_exact, note_text = note_text,
    notes = notes, origin_citations = origin_citations, otu_id = otu_id, 
    tags = tags, taxon_name_classification_id = taxon_name_classification_id,
    taxon_name_id = taxon_name_id, taxon_name_relationship_id = taxon_name_relationship_id, 
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("asserted_distribution_ids", "biological_association_ids", 
    "data_attribute_predicate_id", "data_attribute_value", "image_id",
    "image_id", "keyword_id_and", "keyword_id_or", "otu_id", "taxon_name_classification_id", 
    "taxon_name_id", "taxon_name_relationship_id")

  if (is.null(id)) {
    res <- tw_GET(api_base_url(), "/otus", query = args, csv = csv, ...)
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