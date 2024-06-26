% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tw_observation_matrices.R
\name{tw_observation_matrices}
\alias{tw_observation_matrices}
\alias{tw_obsm}
\title{Observation matrices}
\usage{
tw_observation_matrices(
  observation_matrix_id = NULL,
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)

tw_obsm(
  observation_matrix_id = NULL,
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)
}
\arguments{
\item{observation_matrix_id}{(integer, vector) filter by observation matrix id}

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
Observation matrices

Observation matrices
}
\examples{
\dontrun{
tw_observation_matrices()
}
}
