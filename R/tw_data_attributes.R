#' Data attributes
#'
#' @export
#' @param attribute_subject_id (integer) filter by attribute subject ID
#' @param attribute_subject_type (string) filter by attribute subject type (e.g., TaxonName)
#' @param controlled_vocabulary_term_id (integer, vector) filter by controlled vocabulary term ID
#' @param data_attribute_id (integer, vector) filter by data attribute ID
#' @param type (string) filter by data attribute type
#' @param import_predicate (string) filter by import predicate
#' @param value (string) filter by data attribute value
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_data_attributes()
#' }
tw_data_attributes <- function(
    attribute_subject_id = NULL, attribute_subject_type = NULL, controlled_vocabulary_term_id = NULL,
    data_attribute_id = NULL, type = NULL, import_predicate = NULL, value = NULL,
    csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {
    assert(page, c("numeric", "integer"))
    assert(per, c("numeric", "integer"))

    args <- cc(list(
        attribute_subject_id = attribute_subject_id,
        attribute_subject_type = attribute_subject_type, controlled_vocabulary_term_id = controlled_vocabulary_term_id,
        data_attribute_id = data_attribute_id, type = type, import_predicate = import_predicate, value = value,
        token = token, project_token = project_token,
        page = page, per = per
    ))

    vector_params <- c("controlled_vocabulary_term_id", "data_attribute_id")

    df <- tw_GET(api_base_url(), "/data_attributes", query = args, csv = csv, vector_params = vector_params, ...)
    return(df)
}


#' Data attributes
#' @rdname tw_data_attributes
#' @export
tw_da <- tw_data_attributes
