#' Observations
#'
#' @export
#' @template args
#' @param character_state_id (integer) filter by character state ID
#' @param collection_object_id (integer, vector) filter by collection object ID
#' @param descendants (boolean) filter by descendants
#' @param descriptor_id (integer, vector) filter by descriptor ID
#' @param observation_id (integer, vector) filter by observation ID
#' @param observation_matrix_id (integer, vector) filter by observation matrix ID
#' @param observation_object_global_id (string) filter by observation object global ID
#' @param observation_object_type (string, vector) filter by observation object type (Extract, CollectionObject, Otu)
#' @param observation_type (string, vector) filter by observation type (Observation::Qualitative, Observation::Presence, Observation::Continuous, Observation::Sample, Observation::Media, Observation::FreeText)
#' @param otu_id (integer, vector) filter by OTU ID
#' @param taxon_name_id (integer, vector) filter by taxon name ID
#' @param citations (boolean) filter by observations that have citations
#' @param citation_documents (boolean) filter by observations that have citation documents
#' @param data_depictions (boolean) filter by observations that have data depictions
#' @param depictions (boolean) filter by observations that have depictions
#' @param identifiers (boolean) filter by observations that have identifiers
#' @param notes (boolean) filter by observations that have notes
#' @param note_exact (boolean) filter by exact match on note
#' @param note_text (string) filter by note text
#' @param protocols (boolean) filter by observations that have protocols
#' @param tags (boolean) filter by observations that have tags
#' @return list
#' @examples
#' \dontrun{
#' tw_observations()
#' }
tw_observations <- function(
    character_state_id = NULL, collection_object_id = NULL, descendants = NULL,
    descriptor_id = NULL, observation_id = NULL, observation_matrix_id = NULL,
    observation_object_global_id = NULL, observation_object_type = NULL,
    observation_type = NULL, otu_id = NULL, taxon_name_id = NULL,
    citations = NULL, citation_documents = NULL, data_depictions = NULL,
    depictions = NULL, identifiers = NULL, notes = NULL, note_text = NULL,
    note_exact = NULL, protocols = NULL, tags = NULL, csv = FALSE, token = NULL,
    project_token = NULL, page = 0, per = 50, ...) {
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(
    character_state_id = character_state_id, collection_object_id = collection_object_id,
    descendants = descendants, descriptor_id = descriptor_id, observation_id = observation_id,
    observation_matrix_id = observation_matrix_id, observation_object_global_id = observation_object_global_id,
    observation_object_type = observation_object_type, observation_type = observation_type,
    otu_id = otu_id, taxon_name_id = taxon_name_id, citations = citations, citation_documents = citation_documents,
    data_depictions = data_depictions, depictions = depictions, identifiers = identifiers,
    notes = notes, note_text = note_text, note_exact = note_exact, protocols = protocols,
    tags = tags, token = token, project_token = project_token, page = page, per = per
  ))

  vector_params <- c(
    "collection_object_id", "descriptor_id", "observation_id",
    "observation_matrix_id", "observation_object_type", "otu_id", "taxon_name_id"
  )

  res <- tw_GET(api_base_url(), "/observations", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Observations
#'
#' @rdname tw_observations
#' @export
tw_obs <- tw_observations
