#' Taxon Name Relationships
#'
#' @export
#' @param ancestors (boolean) operate on ancestors of taxon_name_id(s)
#' @param descendants (boolean) operate on descendants of taxon_name_id(s)
#' @param object_taxon_name_id (integer, vector) filter by object taxon name id
#' @param subject_taxon_name_id (integer, vector) filter by subject taxon name id
#' @param taxon_name_id (integer, vector) filter by taxon name id on both subject and object
#' @param taxon_name_relationship_id (integer, vector) filter by taxon name relationship id
#' @param taxon_name_relationship_set (string, vector) filter by taxon name relationship set (synonym, classification, status, misspelling)
#' @param taxon_name_relationship_type (string, vector) filter by taxon name relationship type (e.g., TaxonNameRelationship::Iczn::Invalidating)
#' @param taxon_name_subject_object (string) when 'subject', match taxon_name_id to subjects; when 'object', match to objects; when NULL, either
#' @param citations (boolean) filter by relationships that have citations
#' @param citation_documents (boolean) filter by relationships that have citation documents
#' @param origin_citation (boolean) filter by relationships that have an origin citation
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by relationships that have notes
#' @param verifiers (boolean) filter by relationships that have verifiers
#' @param page (integer) page number
#' @param per (integer) number of records per page
#' @template args
#' @return list
#' @examples
#' \dontrun{
#' tw_taxon_name_relationships()
#' }
tw_taxon_name_relationships <- function(ancestors = NULL, descendants = NULL,
  object_taxon_name_id = NULL, subject_taxon_name_id = NULL, taxon_name_id = NULL,
  taxon_name_relationship_id = NULL, taxon_name_relationship_set = NULL,
  taxon_name_relationship_type = NULL, taxon_name_subject_object = NULL,
  citations = NULL, citation_documents = NULL, origin_citation = NULL,
  note_text = NULL, notes = NULL, verifiers = NULL,
  csv = TRUE, token = NULL, project_token = NULL, page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(ancestors = ancestors, descendants = descendants,
    object_taxon_name_id = object_taxon_name_id,
    subject_taxon_name_id = subject_taxon_name_id, taxon_name_id = taxon_name_id,
    taxon_name_relationship_id = taxon_name_relationship_id,
    taxon_name_relationship_set = taxon_name_relationship_set,
    taxon_name_relationship_type = taxon_name_relationship_type,
    taxon_name_subject_object = taxon_name_subject_object,
    citations = citations, citation_documents = citation_documents,
    origin_citation = origin_citation, note_text = note_text, notes = notes,
    verifiers = verifiers,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("object_taxon_name_id", "subject_taxon_name_id", "taxon_name_id",
                     "taxon_name_relationship_id", "taxon_name_relationship_set",
                     "taxon_name_relationship_type")

  res <- tw_GET(api_base_url(), "/taxon_name_relationships", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}

#' Taxon Name Relationships
#'
#' @rdname tw_taxon_name_relationships
#' @export
tw_tnr <- tw_taxon_name_relationships
