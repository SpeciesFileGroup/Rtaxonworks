#' Taxon determinations
#'
#' A TaxonDetermination records which OTU applies to a specimen (CollectionObject)
#' or field observation (FieldOccurrence), the determiner, and the position in
#' the determination stack (position 1 is the current/accepted determination).
#'
#' @export
#' @template args
#' @param id (integer) access a TaxonDetermination by ID
#' @param taxon_determination_id (integer, vector) filter by TaxonDetermination id
#' @param otu_id (integer, vector) filter by OTU id
#' @param collection_object_id (integer, vector) filter by CollectionObject id
#' @param field_occurrence_id (integer, vector) filter by FieldOccurrence id
#' @param determiner_id (integer, vector) filter by determiner Person id
#' @param taxon_determination_object_id (integer, vector) filter by determined object id
#' @param taxon_determination_object_type (string, vector) filter by determined object type (`CollectionObject`, `FieldOccurrence`)
#' @param extend (string, vector) extend the response
#' @return list
#' @examples
#' \dontrun{
#' tw_taxon_determinations()
#' }
tw_taxon_determinations <- function(id = NULL,
  taxon_determination_id = NULL, otu_id = NULL,
  collection_object_id = NULL, field_occurrence_id = NULL,
  determiner_id = NULL,
  taxon_determination_object_id = NULL, taxon_determination_object_type = NULL,
  extend = NULL, csv = FALSE, token = NULL, project_token = NULL,
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    taxon_determination_id = taxon_determination_id, otu_id = otu_id,
    collection_object_id = collection_object_id,
    field_occurrence_id = field_occurrence_id,
    determiner_id = determiner_id,
    taxon_determination_object_id = taxon_determination_object_id,
    taxon_determination_object_type = taxon_determination_object_type,
    extend = extend,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("taxon_determination_id", "otu_id",
    "collection_object_id", "field_occurrence_id", "determiner_id",
    "taxon_determination_object_id", "taxon_determination_object_type", "extend")

  path <- if (is.null(id)) "/taxon_determinations"
          else sprintf("/taxon_determinations/%d", id)

  res <- tw_GET(api_base_url(), path, query = args, csv = csv,
                vector_params = vector_params, ...)
  return(res)
}

#' Taxon determinations
#' @rdname tw_taxon_determinations
#' @export
tw_td <- tw_taxon_determinations
