% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_abcd_scenario.R
\name{join_abcd_scenario}
\alias{join_abcd_scenario}
\title{Join a data-loanbook object to the abcd and scenario}
\usage{
join_abcd_scenario(
  data,
  abcd,
  scenario,
  region_isos = r2dii.data::region_isos,
  add_green_technologies = FALSE
)
}
\arguments{
\item{data}{A data frame like the output of
\code{r2dii.match::prioritize}.}

\item{abcd}{An asset level data frame like \link[r2dii.data:abcd_demo]{r2dii.data::abcd_demo}.}

\item{scenario}{A scenario data frame like \link[r2dii.data:scenario_demo_2020]{r2dii.data::scenario_demo_2020}.}

\item{region_isos}{A data frame like \link[r2dii.data:region_isos]{r2dii.data::region_isos} (default).}

\item{add_green_technologies}{Logical vector of length 1. \code{FALSE} defaults to
outputting only technologies that are present in both \code{data} and \code{abcd}. Set
to \code{FALSE} to add rows of all possible green technologies (with 0
production).}
}
\value{
Returns a fully joined data frame, linking portfolio, abcd and
scenario.
}
\description{
\code{join_abcd_scenario()} is a simple wrapper of several calls to
\verb{dplyr::join_*()}, forming the master dataset to be used in later steps of
the analysis.
}
\examples{
\dontshow{if (rlang::is_installed("r2dii.data") && rlang::is_installed("r2dii.match", version = "0.1.0")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(r2dii.data)
library(r2dii.match)

valid_matches <- match_name(loanbook_demo, abcd_demo) \%>\%
# WARNING: Remember to validate matches (see `?prioritize`)
  prioritize()

valid_matches \%>\%
  join_abcd_scenario(
  abcd = abcd_demo,
  scenario = scenario_demo_2020,
  region_isos = region_isos_demo
  )
\dontshow{\}) # examplesIf}
}
\seealso{
Other utility functions: 
\code{\link{summarize_weighted_production}()}
}
\concept{utility functions}
