#' Conveyances
#'
#' A Conveyance links a Sound to a polymorphic object (CollectionObject, Otu,
#' FieldOccurrence, etc.), optionally scoped to a time range within the sound
#' recording.
#'
#' @export
#' @template args
#' @param id (integer) access a Conveyance by ID
#' @param conveyance_id (integer, vector) filter by Conveyance id
#' @param sound_id (integer, vector) filter by Sound id
#' @param otu_id (integer, vector) filter by OTU id
#' @param conveyance_object_type (string, vector) filter by conveyed object class
#' @param conveyance_object_id (integer, vector) filter by conveyed object id
#' @param name (string) filter by conveyance name
#' @param extend (string, vector) extend the response (`sound`, `notes`)
#' @return list
#' @examples
#' \dontrun{
#' tw_conveyances()
#' }
tw_conveyances <- function(id = NULL, conveyance_id = NULL, sound_id = NULL,
  otu_id = NULL, conveyance_object_type = NULL, conveyance_object_id = NULL,
  name = NULL, extend = NULL, csv = FALSE, token = NULL, project_token = NULL,
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    conveyance_id = conveyance_id, sound_id = sound_id, otu_id = otu_id,
    conveyance_object_type = conveyance_object_type,
    conveyance_object_id = conveyance_object_id,
    name = name, extend = extend,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("conveyance_id", "sound_id", "otu_id",
    "conveyance_object_type", "conveyance_object_id", "extend")

  path <- if (is.null(id)) "/conveyances" else sprintf("/conveyances/%d", id)

  res <- tw_GET(api_base_url(), path, query = args, csv = csv,
                vector_params = vector_params, ...)
  return(res)
}

#' Conveyances
#' @rdname tw_conveyances
#' @export
tw_conv <- tw_conveyances
