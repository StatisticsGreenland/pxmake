% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_table1.R
\name{subject_code}
\alias{subject_code}
\alias{subject_code.px}
\title{SUBJECT-CODE}
\usage{
subject_code(x, value)

\method{subject_code}{px}(x, value)
}
\arguments{
\item{x}{A px object}

\item{value}{Optional. A character string. If missing, the current SUBJECT-CODE is returned. If NULL, an error is thrown because SUBJECT-CODE cannot be removed.}
}
\value{
A px object or a character string
}
\description{
Inspect or change SUBJECT-CODE. SUBJECT-CODE cannot be removed because it is a mandatory keyword.
}
