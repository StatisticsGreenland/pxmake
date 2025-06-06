% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification.R
\name{px_classification_from_df}
\alias{px_classification_from_df}
\title{px classification from data frame}
\usage{
px_classification_from_df(name, prestext, domain, df)
}
\arguments{
\item{name}{Optional. Name of the classification.}

\item{prestext}{Optional. Presentation text.}

\item{domain}{Optional. Character vector with domain names. Used to link to
PX-file.}

\item{df}{Optional. A data frame with required column 'valuecode' and
optional column 'valuetext', if the codes have texts. Each additional column
represents an aggregation. The column name is the name of the aggregation.
If the column type is character the aggregation levels will be sorted
alphabetically; use factors to control the ordering.}
}
\value{
A classification object
}
\description{
Create px classification from a data frame.
}
\keyword{internal}
