% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tw_authentications.R
\name{tw_user_authenticated}
\alias{tw_user_authenticated}
\title{User authenticated}
\usage{
tw_user_authenticated(
  csv = FALSE,
  token = NULL,
  project_token = NULL,
  page = 0,
  per = 50,
  ...
)
}
\arguments{
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
User authenticated
}
\examples{
\dontrun{
tw_user_authenticated()
}
}
