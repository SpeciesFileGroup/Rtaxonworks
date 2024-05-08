#' Taxon Name Relationships
#'
#' @export
#' @param object_taxon_name_id (integer, vector) filter by object taxon name id
#' @param subject_taxon_name_id (integer, vector) filter by subject taxon name id
#' @param taxon_name_id (integer, vector) filter by taxon name id on both subject and object
#' @param taxon_name_relationship_set (string) filter by taxon name relationship set (synonym, classification, status, and (soon) misspelling)
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
  csv = FALSE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(object_taxon_name_id = object_taxon_name_id, 
    subject_taxon_name_id = subject_taxon_name_id, taxon_name_id = taxon_name_id,
    taxon_name_relationship_set = taxon_name_relationship_set, 
    taxon_name_relationship_type = taxon_name_relationship_type,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("object_taxon_name_id", "subject_taxon_name_id", "taxon_name_id", 
                     "taxon_name_relationship_type", "taxon_name_relationship_type")

  res <- tw_GET(api_base_url(), "/taxon_name_relationships", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}  # TODO: tnr needs a csv endpoint?

#' Taxon Name Relationships
#'
#' @rdname tw_taxon_name_relationships
#' @export
tw_tnr <- tw_taxon_name_relationships
