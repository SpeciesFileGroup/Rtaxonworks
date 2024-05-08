#' People
#'
#' @export
#' @template args
#' @param active_after_year (integer) filter by active after year
#' @param active_before_year (integer) filter by active before year
#' @param born_after_year (integer) filter by born after year
#' @param born_before_year (integer) filter by born before year
#' @param died_after_year (integer) filter by died after year
#' @param died_before_year (integer) filter by died before year
#' @param first_name (string) filter by first name
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param last_name (string) filter by last name
#' @param last_name_starts_with (string) filter by last name starts with
#' @param person_wildcard (vector) filter by person wildcard
#' @param role (vector, string) filter by role
#' @param project_id (integer) filter by project id
#' @param name (string) filter by name
#' @param tags (boolean) filter by people with tags
#' @param user_date_end (string) filter by user date end
#' @param user_date_start (string) filter by user date start
#' @param data_attribute_exact_value (boolean) require exact match on data attribute value
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param data_attributes (boolean) filter by people with data attributes
#' @param note_exact (boolean) require exact match on note text
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by people with notes
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @return list
#' @examples
#' \dontrun{
#' tw_people(last_name="Smith")
#' }
tw_people <- function(
  active_after_year = NULL, active_before_year = NULL, born_after_year = NULL,
  born_before_year = NULL, died_after_year = NULL, died_before_year = NULL,
  first_name = NULL, identifier = NULL, identifier_end = NULL,
  identifier_exact = NULL, identifier_start = NULL, last_name = NULL,
  last_name_starts_with = NULL, person_wildcard = NULL, role = NULL,
  project_id = NULL, name = NULL, tags = NULL, user_date_end = NULL,
  user_date_start = NULL, data_attribute_exact_value = NULL,
  data_attribute_predicate_id = NULL, data_attribute_value = NULL,
  data_attributes = NULL, note_exact = NULL, note_text = NULL,
  notes = NULL, keyword_id_and = NULL, keyword_id_or = NULL,
  csv = FALSE, token = NULL, project_token = NULL, 
  page = 0, per = 50, ...) {
  
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(active_after_year = active_after_year, active_before_year = active_before_year,
    born_after_year = born_after_year, born_before_year = born_before_year,
    died_after_year = died_after_year, died_before_year = died_before_year,
    first_name = first_name, identifier = identifier, identifier_end = identifier_end,
    identifier_exact = identifier_exact, identifier_start = identifier_start,
    last_name = last_name, last_name_starts_with = last_name_starts_with,
    person_wildcard = person_wildcard, role = role, project_id = project_id,
    name = name, tags = tags, user_date_end = user_date_end,
    user_date_start = user_date_start, data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_predicate_id = data_attribute_predicate_id, data_attribute_value = data_attribute_value,
    data_attributes = data_attributes, note_exact = note_exact, note_text = note_text,
    notes = notes, keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("data_attribute_predicate_id", "data_attribute_value", "keyword_id_and", 
                     "keyword_id_or", "person_wildcard", "role")

  res <- tw_GET(api_base_url(), "/people", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
