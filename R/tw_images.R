#' Images
#'
#' @export
#' @template args
#' @param biocuration_class_id (integer, vector) filter by one or more biocuration class IDs
#' @param collection_object_id (integer, vector) filter by one or more collection object IDs
#' @param collection_object_scope (string) filter by collection object scope (e.g., CollectionObject, Observations, CollectingEvent)
#' @param depictions (boolean) filter by images that are depictions
#' @param depiction_object_type (string) filter by depiction object type
#' @param image_id (integer, vector) filter by one or more image IDs
#' @param otu_id (integer, vector) filter by one or more OTU IDs
#' @param citations (boolean) filter by images that have citations
#' @param citation_documents (boolean) filter by images that have citation documents
#' @param origin_citation (boolean) filter by images that have origin citations
#' @param keyword_id_and (integer, vector) filter by one or more keyword IDs (AND)
#' @param keyword_id_or (integer, vector) filter by one or more keyword IDs (OR)
#' @param tags (boolean) filter by images that have tags
#' @return list
#' @examples
#' \dontrun{
#' tw_images(identifier_id=c(1375290,1375299,1375300))
#' }
tw_images <- function(biocuration_class_id = NULL, collection_object_id = NULL,
    collection_object_scope = NULL, depictions = NULL, depiction_object_type = NULL,
    image_id = NULL, otu_id = NULL, citations = NULL, citation_documents = NULL,
    origin_citation = NULL, keyword_id_and = NULL, keyword_id_or = NULL, tags = NULL,
    csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(biocuration_class_id = biocuration_class_id, collection_object_id = collection_object_id,
    collection_object_scope = collection_object_scope, depictions = depictions, depiction_object_type = depiction_object_type,
    image_id = image_id, otu_id = otu_id, citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation, keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or, tags = tags,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or, token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("biocuration_class_id", "collection_object_id", "image_id", "keyword_id_and", "keyword_id_or", "otu_id")

  res <- tw_GET(api_base_url(), "/images", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
