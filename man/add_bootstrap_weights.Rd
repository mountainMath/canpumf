% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pumf.R
\name{add_bootstrap_weights}
\alias{add_bootstrap_weights}
\title{Add bootstrap weights to PUMF data}
\usage{
add_bootstrap_weights(
  pumf_data,
  weight_column,
  boostrap_fraction = 0.8,
  bootstrap_weight_number = 16,
  bootstrap_weight_prefix = "WT",
  seed = NULL
)
}
\arguments{
\item{pumf_data}{A dataframe with PUMF data}

\item{weight_column}{Name of the column with the standard weights}

\item{boostrap_fraction}{Fraction of samples to use for bootstrap}

\item{bootstrap_weight_number}{Number of boostrap weights to generate}

\item{bootstrap_weight_prefix}{Name prefix for the bootstrap weight columns}

\item{seed}{Random see to be used for bootstrap sample for reproducibility}
}
\value{
pumf_base_dir that can be used in the other package functions
}
\description{
Add bootstrap weights to PUMF data
}
