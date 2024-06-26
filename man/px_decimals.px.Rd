% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_table1.R
\name{px_decimals}
\alias{px_decimals}
\alias{px_decimals.px}
\title{DECIMALS}
\usage{
px_decimals(x, value)

\method{px_decimals}{px}(x, value)
}
\arguments{
\item{x}{A px object}

\item{value}{Optional. A character string. If missing, the current DECIMALS is returned. If NULL, an error is thrown because DECIMALS cannot be removed.}
}
\value{
A px object or a character string.
}
\description{
Inspect or change DECIMALS.
}
\examples{
# Set DECIMALS
x1 <-
   px(population_gl) |>
   px_decimals('3')

# Print DECIMALS
px_decimals(x1)
}
\seealso{
\href{https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#\%5B\%7B\%22num\%22\%3A34\%2C\%22gen\%22\%3A0\%7D\%2C\%7B\%22name\%22\%3A\%22XYZ\%22\%7D\%2C184\%2C355\%2C0\%5D}{Statistics Sweden's documentation}
}
