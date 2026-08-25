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
#' @param first_name_like (string) fuzzy filter by first name
#' @param last_name (string) filter by last name
#' @param last_name_like (string) fuzzy filter by last name
#' @param last_name_starts_with (string) filter by last name prefix
#' @param name (string) filter by full name
#' @param prefix (string) filter by prefix
#' @param suffix (string) filter by suffix
#' @param person_id (integer, vector) filter by person id
#' @param role (string, vector) filter by role class name
#' @param with (string, vector) filter to people with a given role
#' @param without (string, vector) filter to people without a given role
#' @param only_project_id (integer, vector) restrict to people in the given projects
#' @param except_project_id (integer, vector) exclude people in the given projects
#' @param except_role (string, vector) exclude people with the given role
#' @param identifier (string) filter by identifier
#' @param identifier_end (string) filter by identifier end
#' @param identifier_exact (boolean) filter by exact match on identifier
#' @param identifier_start (string) filter by identifier start
#' @param identifier_type (string, vector) filter by identifier class type
#' @param identifiers (boolean) filter by people with identifiers
#' @param global_identifiers (boolean) filter by people with global identifiers
#' @param local_identifiers (boolean) filter by people with local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string of identifiers
#' @param match_identifiers_delimiter (string) delimiter for `match_identifiers`
#' @param match_identifiers_type (string) filter by internal or external identifier type
#' @param namespace_id (integer) filter by namespace id
#' @param tags (boolean) filter by people with tags
#' @param exclude_tags (boolean) invert keyword_id_and / keyword_id_or match
#' @param keyword_id_and (integer, vector) filter by keyword id with and operator
#' @param keyword_id_or (integer, vector) filter by keyword id with or operator
#' @param user_date_start (string) filter by user date start (ISO 8601)
#' @param user_date_end (string) filter by user date end (ISO 8601)
#' @param data_attributes (boolean) filter by people with data attributes
#' @param data_attribute_predicate_id (integer, vector) filter by data attribute predicate id
#' @param data_attribute_exact_value (boolean) require exact match on data attribute value
#' @param data_attribute_value (string, vector) filter by data attribute value
#' @param note_exact (boolean) require exact match on note text
#' @param note_text (string) filter by note text
#' @param notes (boolean) filter by people with notes
#' @return list
#' @examples
#' \dontrun{
#' tw_people(last_name = "Smith")
#' tw_people(name = "Darwin", only_project_id = 5)
#' }
tw_people <- function(
  active_after_year = NULL, active_before_year = NULL, born_after_year = NULL,
  born_before_year = NULL, died_after_year = NULL, died_before_year = NULL,
  first_name = NULL, first_name_like = NULL, last_name = NULL,
  last_name_like = NULL, last_name_starts_with = NULL, name = NULL,
  prefix = NULL, suffix = NULL, person_id = NULL, role = NULL,
  with = NULL, without = NULL, only_project_id = NULL, except_project_id = NULL,
  except_role = NULL, identifier = NULL, identifier_end = NULL,
  identifier_exact = NULL, identifier_start = NULL, identifier_type = NULL,
  identifiers = NULL, global_identifiers = NULL, local_identifiers = NULL,
  match_identifiers = NULL, match_identifiers_delimiter = NULL,
  match_identifiers_type = NULL, namespace_id = NULL,
  tags = NULL, exclude_tags = NULL, keyword_id_and = NULL, keyword_id_or = NULL,
  user_date_start = NULL, user_date_end = NULL,
  data_attributes = NULL, data_attribute_predicate_id = NULL,
  data_attribute_exact_value = NULL, data_attribute_value = NULL,
  note_exact = NULL, note_text = NULL, notes = NULL,
  csv = FALSE, token = NULL, project_token = NULL,
  page = 0, per = 50, ...) {

  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))
  if (csv && (!missing(page) || !missing(per))) warn_csv_ignores_paging(per)

  args <- cc(list(
    active_after_year = active_after_year, active_before_year = active_before_year,
    born_after_year = born_after_year, born_before_year = born_before_year,
    died_after_year = died_after_year, died_before_year = died_before_year,
    first_name = first_name, first_name_like = first_name_like,
    last_name = last_name, last_name_like = last_name_like,
    last_name_starts_with = last_name_starts_with, name = name,
    prefix = prefix, suffix = suffix, person_id = person_id, role = role,
    with = with, without = without,
    only_project_id = only_project_id, except_project_id = except_project_id,
    except_role = except_role,
    identifier = identifier, identifier_end = identifier_end,
    identifier_exact = identifier_exact, identifier_start = identifier_start,
    identifier_type = identifier_type, identifiers = identifiers,
    global_identifiers = global_identifiers, local_identifiers = local_identifiers,
    match_identifiers = match_identifiers,
    match_identifiers_delimiter = match_identifiers_delimiter,
    match_identifiers_type = match_identifiers_type, namespace_id = namespace_id,
    tags = tags, exclude_tags = exclude_tags,
    keyword_id_and = keyword_id_and, keyword_id_or = keyword_id_or,
    user_date_start = user_date_start, user_date_end = user_date_end,
    data_attributes = data_attributes,
    data_attribute_predicate_id = data_attribute_predicate_id,
    data_attribute_exact_value = data_attribute_exact_value,
    data_attribute_value = data_attribute_value,
    note_exact = note_exact, note_text = note_text, notes = notes,
    token = token, project_token = project_token, page = page, per = per))

  vector_params <- c("data_attribute_predicate_id", "data_attribute_value",
    "except_project_id", "except_role", "identifier_type",
    "keyword_id_and", "keyword_id_or", "only_project_id", "person_id",
    "role", "with", "without")

  res <- tw_GET(api_base_url(), "/people", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
