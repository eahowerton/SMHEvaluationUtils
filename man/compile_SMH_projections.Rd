% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_SMH_data.R
\name{compile_SMH_projections}
\alias{compile_SMH_projections}
\title{from raw SMH projections, generate one data.table with all
projections after exclusions}
\usage{
compile_SMH_projections(
  proj,
  list2round,
  proj_period_key,
  scenario_round_key,
  rounds_to_include = NA,
  summarize_exclusions = FALSE
)
}
\arguments{
\item{proj}{list of projections where each element is one round}

\item{list2round}{vector key to define which round is in each element of list}

\item{rounds_to_include}{vector of all rounds to include in projections
(if NA, include all projections)}

\item{inc_only}{logical TRUE to include only incident projections}
}
\value{
data.table with all projections included (after exclusions)
}
\description{
from raw SMH projections, generate one data.table with all
projections after exclusions
}
