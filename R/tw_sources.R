#' Sources
#'
#' @export
#' @template args
#' @param author (string) filter by author name
#' @param exact_author (boolean) filter by exact match on the author parameter
#' @param author_id (integer, vector) filter by one or more author IDs
#' @param author_id_or (boolean) filter by author ID with/without OR operator
#' @param bibtex_type (string, vector) filter by BibTeX type (e.g., article, book, inproceedings)
#' @param citation_object_type (string) filter by citation object type (AssertedDistribution, BiologicalAssociation, TaxonName, TaxonNameRelationship, TypeMaterial)
#' @param citations (boolean) filter by sources with/without citations
#' @param citations_on_otus (boolean) used with taxon_name_id to filter by sources with/without citations on the Otu
#' @param documents (boolean) filter by sources with/without documents attached
#' @param empty (string, vector) filter by an fields that should be empty (e.g., c("journal", "year") for records with no journal and year)
#' @param extend (string, vector) extend the response with additional information (e.g., documents, roles)
#' @param not_empty (string, vector) filter by an fields that should not be empty (e.g., c("journal", "year") for records with journal and year)
#' @param global_identifiers (boolean) filter by sources with/without global identifiers
#' @param identifier (string) filter by identifier
#' @param identifier_exact (boolean) filter by exact match on the identifier parameter
#' @param identifier_start (integer) filter by Identifier range start
#' @param identifier_end (integer) filter by Identfier range end
#' @param ids (integer, vector) filter by one or more source IDs (e.g., internal TaxonWorks IDs)
#' @param in_project (boolean) filter by sources in project
#' @param local_identifiers (boolean) filter by sources with/without local identifiers
#' @param match_identifiers (string) match identifiers with a delimited string of identifiers
#' @param match_identifiers_delimiter (string) the delimiter for the match_identifiers parameter (e.g., comma (,), new line (\n), tab (\t), etc.)
#' @param match_identifiers_type (string) the type of indentifiers to match (internal or identifier)
#' @param namespace_id (integer) filter by namespace ID
#' @param nomenclature (boolean) filter by sources with/without nomenclature
#' @param notes (boolean) filter by sources with/without notes
#' @param roles (boolean) filter by sources with/without roles
#' @param title (string) search on the title
#' @param exact_title (boolean) filter by exact match on the title parameter
#' @param query_term (string) search on the full citation
#' @param serial (boolean) filter by sources with/without a serial
#' @param serial_id (integer, vector) filter by one or more serial IDs
#' @param source_type (string) filter by source type (Source::Bibtex, Source::Human, or Source::Verbatim)
#' @param tags (boolean) filter by sources with/without tags
#' @param topic_id (integer, vector) filter by topic ID
#' @param user_id (integer) filter by user ID
#' @param user_target (string) use with user_id to filter on records that were created or updated (created, updated)
#' @param user_date_start (string) filter by user date start (e.g., 2019-01-01)
#' @param user_date_end (string) filter by user date end (e.g., 2019-01-20)
#' @param venn (string) filter with json or browser URL of a query
#' @param venn_mode (string) the mode of the venn filter: exclude (a), intersect (ab), inverse exclude (b)
#' @param with_doi (boolean) filter by sources with/without a DOI
#' @param with_title (boolean) filter by sources with/without BibTeX title
#' @param year_start (integer) filter by year start
#' @param year_end (integer) filter by year end
#' @param any_value_attribute (string, vector) filter by wildcard_attribute (e.g., any of the below fields abstract through year_suffix)
#' @param wildcard_attribute (string, vector) filter by wildcard_attribute (e.g., any of the below fields abstract through year_suffix)
#' @param abstract (string) filter by wildcard_attribute on abstract
#' @param address (string) filter by wildcard_attribute on address
#' @param annote (string) filter by wildcard_attribute on annote
#' @param booktitle (string) filter by wildcard_attribute on booktitle
#' @param chapter (string) filter by wildcard_attribute on chapter
#' @param copyright (string) filter by wildcard_attribute on copyright
#' @param crossref (string) filter by wildcard_attribute on crossref
#' @param doi (string) filter by wildcard_attribute on DOI
#' @param edition (string) filter by wildcard_attribute on edition
#' @param editor (string) filter by wildcard_attribute on editor
#' @param howpublished (string) filter by wildcard_attribute on howpublished
#' @param institution (string) filter by wildcard_attribute on institution
#' @param journal (string) filter by wildcard_attribute on journal
#' @param key (string) filter by wildcard_attribute on key
#' @param language (string) filter by wildcard_attribute on language
#' @param language_id (integer) filter by language ID
#' @param month (string) filter by wildcard_attribute on month
#' @param note (string) filter by wildcard_attribute on note
#' @param number (string) filter by wildcard_attribute on number
#' @param organization (string) filter by wildcard_attribute on organization
#' @param pages (string) filter by wildcard_attribute on pages
#' @param publisher (string) filter by wildcard_attribute on publisher
#' @param school (string) filter by wildcard_attribute on school
#' @param series (string) filter by wildcard_attribute on series
#' @param stated_year (string) filter by wildcard_attribute on stated_year
#' @param translator (string) filter by wildcard_attribute on translator
#' @param type (string) filter by wildcard_attribute on type
#' @param url (string) filter by wildcard_attribute on URL
#' @param verbatim (string) filter by wildcard_attribute on verbatim
#' @param verbatim_contents (string) filter by wildcard_attribute on verbatim_contents
#' @param verbatim_keywords (string) filter by wildcard_attribute on verbatim_keywords
#' @param volume (string) filter by wildcard_attribute on volume
#' @param year_suffix (string) filter by wildcard_attribute on year_suffix
#' @return list
#' @examples
#' \dontrun{
#' tw_sources(author="Smith")
#' }
tw_sources <- function(author = NULL, exact_author = NULL, author_id = NULL,
    author_id_or = NULL, bibtex_type = NULL, citations = NULL,
    citation_object_type = NULL, documents = NULL, citations_on_otus = NULL,
    empty = NULL, not_empty = NULL, extend = NULL, global_identifiers = NULL, identifier = NULL,
    identifier_exact = NULL, identifier_start = NULL, identifier_end = NULL, ids = NULL, 
    in_project = NULL, local_identifiers = NULL, match_identifiers = NULL, 
    match_identifiers_delimiter = NULL, match_identifiers_type = NULL, nomenclature = NULL,
    notes = NULL, roles = NULL, serial = NULL, serial_id = NULL, source_type = NULL,
    tags = NULL, title = NULL, exact_title = NULL, query_term = NULL, user_id = NULL, user_target = NULL,
    user_date_start = NULL, user_date_end = NULL,venn = NULL, venn_mode = NULL,
    with_doi = NULL, with_title = NULL, topic_id = NULL,
    year_start = NULL, year_end = NULL, wildcard_attribute = NULL, abstract = NULL,
    address = NULL, annote = NULL, booktitle = NULL, chapter = NULL, copyright = NULL,
    crossref = NULL, doi = NULL, edition = NULL, editor = NULL, howpublished = NULL,
    institution = NULL, journal = NULL, key = NULL, language = NULL, language_id = NULL, month = NULL,
    note = NULL, number = NULL, organization = NULL, pages = NULL, publisher = NULL,
    school = NULL, series = NULL, stated_year = NULL, translator = NULL, type = NULL,
    url = NULL, verbatim = NULL, verbatim_contents = NULL, verbatim_keywords = NULL,
    volume = NULL, year_suffix = NULL, csv = FALSE, token = NULL, project_token = NULL, 
    page = 0, per = 50, ...) {
  
  assert(page, c("numeric", "integer"))
  assert(per, c("numeric", "integer"))

  args <- cc(list(author = author, exact_author = exact_author, author_id = author_id,
    author_id_or = author_id_or, bibtex_type = bibtex_type, citations = citations,
    citation_object_type = citation_object_type, citations_on_otus = citations_on_otus, documents = documents,
    empty = empty, not_empty = not_empty, extend = extend, global_identifiers = global_identifiers, identifier = identifier,
    identifier_exact = identifier_exact, identifier_start = identifier_start, identifier_end = identifier_end, ids = ids,
    in_project = in_project, local_identifiers = local_identifiers, match_identifiers = match_identifiers, 
    match_identifiers_delimiter = match_identifiers_delimiter, match_identifiers_type = match_identifiers_type, 
    nomenclature = nomenclature,
    notes = notes, roles = roles, serial = serial, serial_id = serial_id, source_type = source_type,
    tags = tags, title = title, exact_title = exact_title, query_term = query_term, user_id = user_id, user_target = user_target,
    user_date_start = user_date_start, user_date_end = user_date_end,
    venn = venn, venn_mode = venn_mode, with_doi = with_doi, with_title = with_title, topic_id = topic_id,
    year_start = year_start, year_end = year_end, wildcard_attribute = wildcard_attribute, abstract = abstract,
    address = address, annote = annote, booktitle = booktitle, chapter = chapter, copyright = copyright,
    crossref = crossref, doi = doi, edition = edition, editor = editor, howpublished = howpublished,
    institution = institution, journal = journal, key = key, language = language, language_id = language_id, month = month,
    note = note, number = number, organization = organization, pages = pages, publisher = publisher,
    school = school, series = series, stated_year = stated_year, translator = translator, type = type,
    url = url, verbatim = verbatim, verbatim_contents = verbatim_contents, verbatim_keywords = verbatim_keywords,
    volume = volume, year_suffix = year_suffix, token = token, project_token = project_token, page = page, per = per))

  vector_params = c("author_id", "bibtex_type", "citation_object_type", "empty", "ids", "keyword_id_and", "keyword_id_or", 
    "not_empty", "serial_id", "source_id", "taxon_name_id", "topic_id")

  res <- tw_GET(api_base_url(), "/sources", query = args, csv = csv, vector_params = vector_params, ...)
  return(res)
}
