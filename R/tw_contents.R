#' Contents
#'
#' @export
#' @template args
#' @param content_id (integer, vector) filter by content id
#' @param citations (boolean) filter by contents that have citations
#' @param citation_documents (boolean) filter by contents that have citation documents
#' @param origin_citation (boolean) filter by contents that have an origin citation
#' @param confidences (boolean) filter by contents that have confidences
#' @param confidence_level_id (integer, vector) filter by confidence level id
#' @param without_confidence_level_id (integer, vector) filter by contents without these confidence level ids
#' @param exclude_confidences (boolean) exclude contents with confidences
#' @param data_attributes (boolean) filter by contents that have data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate (CVT) ids
#' @param data_attribute_without_predicate_id (integer, vector) filter by contents without these data attribute predicate (CVT) ids
#' @param data_attribute_exact_value (string, vector) filter by exact data attribute values
#' @param data_attribute_wildcard_value (string, vector) filter by wildcarded data attribute values
#' @param data_attribute_exact_pair (string, vector) filter by exact predicate:value pairs (format 'predicate_id:value')
#' @param data_attribute_wildcard_pair (string, vector) filter by wildcarded predicate:value pairs (format 'predicate_id:value')
#' @param depictions (boolean) filter by contents that have depictions
#' @param data_depictions (boolean) filter by contents with data depictions
#' @param exact (boolean) require exact match on search term
#' @param image_id (integer, vector) filter by image id
#' @param images (boolean) filter by contents that have images
#' @param otu_id (integer, vector) filter by otu id
#' @param text (string) filter by text
#' @param topic_id (integer, vector) filter by topic id
#' @return list
#' @examples
#' \dontrun{
#' tw_contents()
#' }
tw_contents <- function(
    content_id = NULL, citations = NULL, citation_documents = NULL,
    origin_citation = NULL, confidences = NULL, confidence_level_id = NULL,
    without_confidence_level_id = NULL, exclude_confidences = NULL,
    data_attributes = NULL, data_attribute_predicate_id = NULL,
    data_attribute_without_predicate_id = NULL, data_attribute_exact_value = NULL,
    data_attribute_wildcard_value = NULL, data_attribute_exact_pair = NULL,
    data_attribute_wildcard_pair = NULL, depictions = NULL, data_depictions = NULL,
    exact = NULL, image_id = NULL, images = NULL, otu_id = NULL, text = NULL,
    topic_id = NULL, csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {
    assert(page, c("numeric", "integer"))
    assert(per, c("numeric", "integer"))

    args <- cc(list(
        content_id = content_id, citations = citations,
        citation_documents = citation_documents, origin_citation = origin_citation,
        confidences = confidences, confidence_level_id = confidence_level_id,
        without_confidence_level_id = without_confidence_level_id,
        exclude_confidences = exclude_confidences,
        data_attributes = data_attributes,
        data_attribute_predicate_id = data_attribute_predicate_id,
        data_attribute_without_predicate_id = data_attribute_without_predicate_id,
        data_attribute_exact_value = data_attribute_exact_value,
        data_attribute_wildcard_value = data_attribute_wildcard_value,
        data_attribute_exact_pair = data_attribute_exact_pair,
        data_attribute_wildcard_pair = data_attribute_wildcard_pair,
        depictions = depictions, data_depictions = data_depictions,
        exact = exact, image_id = image_id, images = images, otu_id = otu_id,
        text = text, topic_id = topic_id,
        token = token, project_token = project_token,
        page = page, per = per
    ))

    vector_params <- c("content_id", "confidence_level_id", "without_confidence_level_id",
        "data_attribute_predicate_id", "data_attribute_without_predicate_id",
        "data_attribute_exact_value", "data_attribute_wildcard_value",
        "data_attribute_exact_pair", "data_attribute_wildcard_pair",
        "image_id", "otu_id", "topic_id")

    df <- tw_GET(api_base_url(), "/contents", query = args, csv = csv, vector_params = vector_params, ...)
    return(df)
}
