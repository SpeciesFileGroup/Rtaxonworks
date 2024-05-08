#' Notes
#'
#' @export
#' @template args
#' @param note_id (integer, vector) filter by note id
#' @param note_object_id (integer, vector) filter by note object id
#' @param note_object_type (string, vector) filter by note object type
#' @param text (string) filter by note text
#' @param polymorphic (boolean) filter by polymorphic notes
#' @return list
#' @examples
#' \dontrun{
#' tw_notes(last_name="Smith")
#' }
tw_notes <- function(note_id = NULL, note_object_id = NULL,
                     note_object_type = NULL, text = NULL, polymorphic = NULL,
                     csv = FALSE, token = NULL, project_token = NULL,
                     page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(note_id = note_id, note_object_id = note_object_id,
                  note_object_type = note_object_type, text = text,
                  polymorphic = polymorphic, token = token,
                  project_token = project_token, page = page, per = per))

  vector_params <- c("note_id", "note_object_id", "note_object_type")

  res <- tw_GET(api_base_url(), "/notes", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}


#' Notes
#'
#' @rdname tw_notes
#' @export
tw_note <- tw_notes
