% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tw_taxon_names.R
\name{tw_taxon_names}
\alias{tw_taxon_names}
\alias{tw_tn}
\title{Taxon Names}
\usage{
tw_taxon_names(
  ancestors = NULL,
  author = NULL,
  author_exact = NULL,
  authors = NULL,
  citations = NULL,
  citation_documents = NULL,
  collecting_event_id = NULL,
  collection_object_id = NULL,
  combination_taxon_name_id = NULL,
  data_attribute_exact_value = NULL,
  data_attribute_predicate_id = NULL,
  data_attribute_value = NULL,
  data_attributes = NULL,
  descendants = NULL,
  descendants_max_depth = NULL,
  etymology = NULL,
  exact = NULL,
  image_id = NULL,
  images = NULL,
  keyword_id_and = NULL,
  keyword_id_or = NULL,
  leaves = NULL,
  name = NULL,
  name_exact = NULL,
  note_exact = NULL,
  note_text = NULL,
  notes = NULL,
  nomenclature_code = NULL,
  nomenclature_group = NULL,
  not_specified = NULL,
  origin_citation = NULL,
  original_combination = NULL,
  otus = NULL,
  parent_id = NULL,
  rank = NULL,
  tags = NULL,
  taxon_name_author_id_or = NULL,
  taxon_name_id = NULL,
  taxon_name_relationship = NULL,
  taxon_name_relationship_type = NULL,
  taxon_name_type = NULL,
  type_metadata = NULL,
  validity = NULL,
  year = NULL,
  year_end = NULL,
  year_start = NULL,
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)

tw_tn(
  ancestors = NULL,
  author = NULL,
  author_exact = NULL,
  authors = NULL,
  citations = NULL,
  citation_documents = NULL,
  collecting_event_id = NULL,
  collection_object_id = NULL,
  combination_taxon_name_id = NULL,
  data_attribute_exact_value = NULL,
  data_attribute_predicate_id = NULL,
  data_attribute_value = NULL,
  data_attributes = NULL,
  descendants = NULL,
  descendants_max_depth = NULL,
  etymology = NULL,
  exact = NULL,
  image_id = NULL,
  images = NULL,
  keyword_id_and = NULL,
  keyword_id_or = NULL,
  leaves = NULL,
  name = NULL,
  name_exact = NULL,
  note_exact = NULL,
  note_text = NULL,
  notes = NULL,
  nomenclature_code = NULL,
  nomenclature_group = NULL,
  not_specified = NULL,
  origin_citation = NULL,
  original_combination = NULL,
  otus = NULL,
  parent_id = NULL,
  rank = NULL,
  tags = NULL,
  taxon_name_author_id_or = NULL,
  taxon_name_id = NULL,
  taxon_name_relationship = NULL,
  taxon_name_relationship_type = NULL,
  taxon_name_type = NULL,
  type_metadata = NULL,
  validity = NULL,
  year = NULL,
  year_end = NULL,
  year_start = NULL,
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)
}
\arguments{
\item{ancestors}{(boolean) include ancestors}

\item{author}{(string) filter by cached_author_year}

\item{author_exact}{(boolean) filter by exact match on cached_author_year}

\item{authors}{(boolean) filter by taxon names that have an author string present}

\item{citations}{(boolean) filter by taxon names with citations}

\item{citation_documents}{(boolean) filter by taxon names with citation documents}

\item{collecting_event_id}{(integer) filter by collecting event id}

\item{collection_object_id}{(integer) filter by collection object id}

\item{combination_taxon_name_id}{(integer) filter by combination taxon name id}

\item{data_attribute_exact_value}{(boolean) filter by exact match on data attribute value}

\item{data_attribute_predicate_id}{(integer) filter by data attribute predicate id}

\item{data_attribute_value}{(string) filter by data attribute value}

\item{data_attributes}{(boolean) filter by taxon names that have 1 or more data attributes}

\item{descendants}{(boolean) include descendants}

\item{descendants_max_depth}{(integer) maximum depth of descendants to include}

\item{etymology}{(boolean) filter by taxon names with etymology}

\item{exact}{(boolean) filter by exact match on name}

\item{image_id}{(integer) filter by taxon names associated with an image id}

\item{images}{(boolean) filter by taxon names with images}

\item{keyword_id_and}{(integer) filter by keyword id with and operator}

\item{keyword_id_or}{(integer) filter by keyword id with or operator}

\item{leaves}{(boolean) filiter by names having no descendants}

\item{name}{(string, vector) filter by name}

\item{name_exact}{(boolean) filter by exact match on name}

\item{note_exact}{(boolean) filter by exact match on note}

\item{note_text}{(string) filter by note text}

\item{notes}{(boolean) filter by taxon names with notes}

\item{nomenclature_code}{(string) filter by nomenclatural code}

\item{nomenclature_group}{(string) filter by nomenclature group}

\item{not_specified}{(boolean) filter by whether the name has NOT SPECIFIED in one of the cached values}

\item{origin_citation}{(boolean) filter by taxon names with an origin citation}

\item{original_combination}{(boolean) filter by names with at least one element of the original combination}

\item{otus}{(boolean) filter by names that have one or more Otus}

\item{parent_id}{(integer) filter by parent id}

\item{rank}{(string) filter by rank class (e.g., NomenclaturalRank::Iczn::SpeciesGroup::Species)}

\item{tags}{(boolean) filter by taxon names with tags}

\item{taxon_name_author_id_or}{(boolean)}

\item{taxon_name_id}{(integer) filter by taxon name id}

\item{taxon_name_relationship}{(string) filter by taxon name relationship}

\item{taxon_name_relationship_type}{(string) filter by taxon name relationship type (e.g., TaxonNameRelationship::Iczn::Invalidating)}

\item{taxon_name_type}{(string) filter by taxon name type (e.g., Protonym, Combination, Hybrid)}

\item{type_metadata}{(boolean) filter by taxon names that are linked to type specimen material}

\item{validity}{(boolean) filter by taxon names that are valid}

\item{year}{(integer) filter by authority year in the cached_author_year field}

\item{year_end}{(integer) filter by taxon names with the cached_author_year field less than or equal to this value}

\item{year_start}{(integer) filter by taxon names with the cached_author_year field greater than or equal to this value}

\item{token}{(string) a user token (set with the TW_USER_TOKEN environment variable)}

\item{project_token}{(string) a project token (set with the TW_PROJECT_TOKEN environment variable)}

\item{page}{(integer) requested number of offset records (Default: 0)}

\item{per}{(integer) requested number of maximum records to be returned (Default: 50, Maximum: 10000)}

\item{...}{curl options passed on to \code{\link[httr2]{verb-GET}}}
}
\value{
list
}
\description{
Taxon Names

Taxon Names
}
\examples{
\dontrun{
tw_taxon_names(name="Lycorma delicatula", valid=TRUE)
}
}
