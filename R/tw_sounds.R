#' Sounds
#'
#' Sound recordings (audio equivalent of Image), linked to collection objects
#' or field occurrences via Conveyances.
#'
#' @export
#' @template args
#' @param id (integer) access a Sound by ID
#' @param sound_id (integer, vector) filter by Sound id
#' @param name (string) filter by sound name
#' @param otu_id (integer, vector) filter by OTU id (via conveyances)
#' @param collection_object_id (integer, vector) filter by CollectionObject id
#' @param field_occurrence_id (integer, vector) filter by FieldOccurrence id
#' @param conveyances (boolean) filter by whether the sound has conveyances
#' @param with_name (boolean) filter by whether the sound has a name
#' @param extend (string, vector) extend the response (`conveyances`, `attribution`, `notes`)
#' @return list
#' @examples
#' \dontrun{
#' tw_sounds()
#' }
tw_sounds <- function(id = NULL, sound_id = NULL, name = NULL, otu_id = NULL,
                      collection_object_id = NULL, field_occurrence_id = NULL,
                      conveyances = NULL, with_name = NULL, extend = NULL,
                      csv = FALSE, token = NULL, project_token = NULL,
                      page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    sound_id = sound_id, name = name, otu_id = otu_id,
    collection_object_id = collection_object_id,
    field_occurrence_id = field_occurrence_id,
    conveyances = conveyances, with_name = with_name, extend = extend,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("sound_id", "otu_id", "collection_object_id",
                     "field_occurrence_id", "extend")

  path <- if (is.null(id)) "/sounds" else sprintf("/sounds/%d", id)

  res <- tw_GET(api_base_url(), path, query = args, csv = csv,
                vector_params = vector_params, ...)
  return(res)
}

#' Sounds
#' @rdname tw_sounds
#' @export
tw_snd <- tw_sounds
