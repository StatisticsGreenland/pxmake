% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_totals.R
\name{add_totals}
\alias{add_totals}
\alias{add_totals.px}
\title{Add 'total' level to variables}
\usage{
add_totals(x, variables)

\method{add_totals}{px}(x, variables)
}
\arguments{
\item{x}{A px object}

\item{variables}{List of variables to add total levels to.}
}
\value{
A px object
}
\description{
Add 'total' level to multiple variables. The name of the total level is set
as 'elimination' in px$variables2, otherwise 'Total' is used. The value of
is the sum of the figures variable. NAs are ignored when summing.
}
