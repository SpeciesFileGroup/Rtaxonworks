#' Contents
#'
#' @export
#' @template args
#' @param citations (boolean) filter by contents that have citations
#' @param citation_documents (boolean) filter by contents that have citation documents
#' @param origin_citation (boolean) filter by contents that have an origin citation
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
    citations = NULL, citation_documents = NULL, origin_citation = NULL,
    exact = NULL, image_id = NULL, images = NULL, otu_id = NULL, text = NULL,
    topic_id = NULL, csv = FALSE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {
    assert(page, c("numeric", "integer"))
    assert(per, c("numeric", "integer"))

    args <- cc(list(
        citations = citations, citation_documents = citation_documents,
        origin_citation = origin_citation, exact = exact, image_id = image_id,
        images = images, otu_id = otu_id, text = text, topic_id = topic_id,
        token = token, project_token = project_token,
        page = page, per = per
    ))

    vector_params <- c("image_id", "otu_id", "topic_id")

    df <- tw_GET(api_base_url(), "/contents", query = args, csv = csv, vector_params = vector_params, ...)
    return(df)
}
