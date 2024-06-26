% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tw_common_names.R
\name{tw_common_names}
\alias{tw_common_names}
\alias{tw_cn}
\title{Common names}
\usage{
tw_common_names(
  common_name_id = NULL,
  geographic_area_id = NULL,
  language_id = NULL,
  name = NULL,
  otu_id = NULL,
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)

tw_cn(
  common_name_id = NULL,
  geographic_area_id = NULL,
  language_id = NULL,
  name = NULL,
  otu_id = NULL,
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)
}
\arguments{
\item{common_name_id}{(integer, vector) filter by common name id}

\item{geographic_area_id}{(integer) filter by geographic area id}

\item{language_id}{(integer) filter by language id}

\item{name}{(string) filter by name}

\item{otu_id}{(integer, vector) filter by otu id}

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
Common names

Common names
}
\examples{
\dontrun{
tw_common_names()
}
}
