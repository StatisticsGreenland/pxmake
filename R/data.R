#' Population Greenland
#'
#' A subset of the population count data available in Statistic Greenland's
#' BEESTA table.
#'
#' @source <https://bank.stat.gl/pxweb/en/Greenland/Greenland__BE__BE01__BE0120/BEXSTA.px/>
"population_gl"

#' Greenlanders
#'
#' A fictive data set with demographic data for Greenlanders split in two cohorts.
#'
"greenlanders"

#' px keywords
#'
#' Properties of all px keywords. Used internally by the package.
#'
#' @format A data frame:
#' \describe{
#'   \item{keyword}{Name}
#'   \item{mandatory}{Is required in a px file}
#'   \item{table_meta}{Is metadata for entire table; not individual variables or cells}
#'   \item{language_dependent}{Is language dependent}
#'   \item{indexed_by_contvariable}{Is indexed if CONTVARIABLE is set}
#'   \item{quote_value}{Value should be quoted in px file}
#'   \item{default_value}{Default value for mandatory keywords}
#'   \item{order}{Recommended order}
#' }
#'
#'
#' @source <https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf>
"px_keywords"
