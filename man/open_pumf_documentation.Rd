% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumf_documentation.R
\name{open_pumf_documentation}
\alias{open_pumf_documentation}
\title{Open PUMF documentation in browser}
\usage{
open_pumf_documentation(
  pumf_series,
  pumf_version = NULL,
  documentation_type = "user_guide",
  pumf_cache_path = getOption("canpumf.cache_path")
)
}
\arguments{
\item{pumf_series}{sereis for the pumf data, like LSF, or CHS}

\item{pumf_version}{In case there are several versions of a given series, like for LFS, the version}

\item{documentation_type}{which documentation to open, either "user_guide", "reference_guide" or "quality" or "errata". Not
all types are available for all PUMFs.
identifiers. For LFS this is the month/year.}

\item{pumf_cache_path}{A path to a permanent cache. If none is fould the data is stored in the temporary
directory for the duration of the session.}
}
\value{
nothing, opens document in browser
}
\description{
When available this will open the PUF user guide in the default browser.
}
