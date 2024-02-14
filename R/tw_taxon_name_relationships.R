#' Taxon Name Relationships
#'
#' @export
#' @importFrom tibble as_tibble
#' @param object_taxon_name_id (integer, vector) filter by object taxon name id
#' @param subject_taxon_name_id (integer, vector) filter by subject taxon name id
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param taxon_name_relationship_set (string) filter by taxon name relationship set ('validating', 'invalidating', 'exceptions')
#' @param taxon_name_relationship_type (string, vector) filter by taxon name relationship type (e.g., TaxonNameRelationship::Iczn::Invalidating)
#' @param page (integer) page number
#' @param per (integer) number of records per page
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_taxon_name_relationships()
#' }
tw_taxon_name_relationships <- function(object_taxon_name_id = NULL, 
  subject_taxon_name_id = NULL, taxon_name_id = NULL,
  taxon_name_relationship_set = NULL, taxon_name_relationship_type = NULL,
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(object_taxon_name_id = object_taxon_name_id, 
    subject_taxon_name_id = subject_taxon_name_id, taxon_name_id = taxon_name_id,
    taxon_name_relationship_set = taxon_name_relationship_set, 
    taxon_name_relationship_type = taxon_name_relationship_type,
    page = page, per = per))

  res <- tw_GET(api_base_url(), "/taxon_name_relationships", body = args, ...)
  df <- as_tibble(tw_list_to_df(res))
  return(df)
}

tw_tnr <- tw_taxon_name_relationships
