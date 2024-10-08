% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{px_keywords}
\alias{px_keywords}
\title{px keywords}
\format{
A data frame:
\describe{
\item{keyword}{Name}
\item{mandatory}{Is required in a px file}
\item{table_meta}{Is metadata for entire table; not individual variables or cells}
\item{language_dependent}{Is language dependent}
\item{indexed_by_contvariable}{Is indexed if CONTVARIABLE is set}
\item{quote_value}{Value should be quoted in px file}
\item{default_value}{Default value for mandatory keywords}
\item{documentation}{URL to Statistic Sweden's documentation}
\item{order}{Recommended order}
}
}
\source{
\url{https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf}
}
\usage{
px_keywords
}
\description{
Properties of all px keywords. Used internally by the package.
}
\keyword{datasets}
