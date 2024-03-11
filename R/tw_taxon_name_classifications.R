#' Taxon Name Classifications
#'
#' @export
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_classification_type (string, vector) filter by taxon name classification type (e.g., TaxonNameClassification::Iczn::Available)
#' @param taxon_name_classification_set (string, vector) filter by taxon name classification set ('validating', 'invalidating', 'exceptions')
#' @param page (integer) page number
#' @param per (integer) number of records per page
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_taxon_name_classifications()
#' }
tw_taxon_name_classifications <- function(taxon_name_id = NULL,
  taxon_name_classification_type = NULL, taxon_name_classification_set = NULL,
  csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(taxon_name_id = taxon_name_id,
    taxon_name_classification_type = taxon_name_classification_type,
    taxon_name_classification_set = taxon_name_classification_set,
    token = token, project_token = project_token, page = page, per = per))

  res <- tw_GET(api_base_url(), "/taxon_name_classifications", query = args, csv = csv, ...)
  return(res)
}
# TODO: tnc needs a csv endpoint?

#' Taxon Name Classifications
#'
#' @rdname tw_taxon_name_classifications
#' @export
tw_tnc <- tw_taxon_name_classifications
