#' Otus
#'
#' @export
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
#' @param taxon_name_classification_ids (string) filter by taxon name classification ids
#' @param taxon_name_id (integer) filter by taxon name id
#' @param taxon_name_relationship_id (string) filter by taxon name relationship ids
#' @template args
#' @return list
#' @examples
#' tw_otus()
tw_otus <- function(asserted_distribution_ids = NULL, biological_association_ids = NULL,
  citations = NULL, citations_documents = NULL, data_attribute_exact_value = NULL,
  data_attribute_predicate_id = NULL, data_attribute_value = NULL, data_attributes = NULL,
  data_attributes_attributes = NULL, image_id = NULL, images = NULL, keyword_id_and = NULL,
  keyword_id_or = NULL, name = NULL, note_exact = NULL, note_text = NULL, notes = NULL,
  origin_citations = NULL, otu_id = NULL, otu_ids = NULL, tags = NULL,
  taxon_name_classification_ids = NULL, taxon_name_id = NULL, taxon_name_ids = NULL,
  taxon_name_relationship_ids = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(asserted_distribution_ids = asserted_distribution_ids,
    biological_association_ids = biological_association_ids, citations = citations,
    citations_documents = citations_documents, data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_predicate_id = data_attribute_predicate_id, data_attribute_value = data_attribute_value,
    data_attributes = data_attributes, data_attributes_attributes = data_attributes_attributes,
    image_id = image_id, images = images, keyword_id_and = keyword_id_and,
    keyword_id_or = keyword_id_or, name = name, note_exact = note_exact, note_text = note_text,
    notes = notes, origin_citations = origin_citations, otu_id = otu_id, otu_ids = otu_ids,
    tags = tags, taxon_name_classification_ids = taxon_name_classification_ids,
    taxon_name_id = taxon_name_id, taxon_name_ids = taxon_name_ids,
    taxon_name_relationship_ids = taxon_name_relationship_ids, page = page, per = per))

  tmp <- tw_GET(api_base_url(), "api/v1/otus", query = args, ...)
  tmp <- tibble::as_tibble(tmp)
  return(tmp)
}
