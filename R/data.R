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
#'   \item{mandatory}{Is required in a PX-file}
#'   \item{table_meta}{Is metadata for entire table; not individual variables or cells}
#'   \item{language_dependent}{Is language dependent}
#'   \item{indexed_by_contvariable}{Is indexed if CONTVARIABLE is set}
#'   \item{quote_value}{Value should be quoted in PX-file}
#'   \item{default_value}{Default value for mandatory keywords}
#'   \item{documentation}{URL to Statistic Sweden's documentation}
#'   \item{order}{Recommended order}
#' }
#'
#'
#' @source <https://www.scb.se/globalassets/vara-tjanster/px-programmen/PX-file_format_specification_2013.pdf>
"px_keywords"

#' Age classification
#'
#' Example data set to create age classification with aggreations form 10 and
#' 25 years classes.
#'
#' @format A data frame:
#' \describe{
#'   \item{valuecode}{Value code}
#'   \item{valuetext}{Value text}
#'   \item{10-years_classes}{Aggregtation into 10 years classes}
#'   \item{25-years_classes}{Aggregtation into 25 years classes}
#' }
"age_classification"
