#' Darwin Core occurrences
#'
#' @export
#' @template args
#' @param dwc_occurrence_id (integer, vector) filter by DwC occurrence internal id
#' @param dwc_occurrence_object_id (integer, vector) filter by object id (the record the DwC row is derived from)
#' @param dwc_occurrence_object_type (string, vector) filter by object type (e.g., `CollectionObject`, `AssertedDistribution`, `FieldOccurrence`)
#' @param otu_id (integer, vector) filter by otu id
#' @param person_id (integer, vector) filter by person id (via any person role)
#' @param taxon_name_id (integer, vector) filter by taxon name id
#' @param empty_rank (string, vector) return records missing values at the given rank(s)
#' @param scientificName (string) filter by DwC scientificName
#' @param basisOfRecord (string) filter by DwC basisOfRecord
#' @param catalogNumber (string) filter by DwC catalogNumber
#' @param collectionCode (string) filter by DwC collectionCode
#' @param country (string) filter by DwC country
#' @param stateProvince (string) filter by DwC stateProvince
#' @param county (string) filter by DwC county
#' @param eventDate (string) filter by DwC eventDate
#' @param recordedBy (string) filter by DwC recordedBy
#' @param kingdom (string) filter by DwC kingdom
#' @param phylum (string) filter by DwC phylum
#' @param dwcClass (string) filter by DwC class (renamed to avoid clash with Ruby keyword)
#' @param order (string) filter by DwC order
#' @param family (string) filter by DwC family
#' @param genus (string) filter by DwC genus
#' @param occurrenceID (string) filter by DwC occurrenceID
#' @param any_value_attribute (string, vector) filter by any wildcard-matched attribute value
#' @param wildcard_attribute (string, vector) fields to search when using `any_value_attribute`
#' @return list
#' @examples
#' \dontrun{
#' tw_dwc_occurrences()
#' tw_dwc_occurrences(scientificName = "Alnetoidia alneti")
#' }
tw_dwc_occurrences <- function(
    dwc_occurrence_id = NULL, dwc_occurrence_object_id = NULL,
    dwc_occurrence_object_type = NULL, otu_id = NULL, person_id = NULL,
    taxon_name_id = NULL, empty_rank = NULL,
    scientificName = NULL, basisOfRecord = NULL, catalogNumber = NULL,
    collectionCode = NULL, country = NULL, stateProvince = NULL, county = NULL,
    eventDate = NULL, recordedBy = NULL, kingdom = NULL, phylum = NULL,
    dwcClass = NULL, order = NULL, family = NULL, genus = NULL,
    occurrenceID = NULL, any_value_attribute = NULL, wildcard_attribute = NULL,
    csv = TRUE, token = NULL, project_token = NULL,
    page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    dwc_occurrence_id = dwc_occurrence_id,
    dwc_occurrence_object_id = dwc_occurrence_object_id,
    dwc_occurrence_object_type = dwc_occurrence_object_type,
    otu_id = otu_id, person_id = person_id, taxon_name_id = taxon_name_id,
    empty_rank = empty_rank,
    scientificName = scientificName, basisOfRecord = basisOfRecord,
    catalogNumber = catalogNumber, collectionCode = collectionCode,
    country = country, stateProvince = stateProvince, county = county,
    eventDate = eventDate, recordedBy = recordedBy,
    kingdom = kingdom, phylum = phylum, class = dwcClass, order = order,
    family = family, genus = genus, occurrenceID = occurrenceID,
    any_value_attribute = any_value_attribute,
    wildcard_attribute = wildcard_attribute,
    token = token, project_token = project_token,
    page = page, per = per
  ))

  vector_params <- c("dwc_occurrence_id", "dwc_occurrence_object_id",
    "dwc_occurrence_object_type", "empty_rank", "otu_id", "person_id",
    "taxon_name_id", "any_value_attribute", "wildcard_attribute")

  res <- tw_GET(api_base_url(), "/dwc_occurrences", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Darwin Core occurrences
#'
#' @rdname tw_dwc_occurrences
#' @export
tw_dwc <- tw_dwc_occurrences
