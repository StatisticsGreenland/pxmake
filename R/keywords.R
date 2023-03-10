#' Get PX keywords
#' @return data frame with all PX keywords
get_px_keywords <- function() {
  tibble::tribble(
                                 ~keyword, ~mandatory,
                                "CHARSET",      FALSE,
                           "AXIS-VERSION",      FALSE,
                               "CODEPAGE",      FALSE,
                               "LANGUAGE",      FALSE,
                              "LANGUAGES",      FALSE,
                          "CREATION-DATE",      FALSE,
                            "NEXT-UPDATE",      FALSE,
                              "PX-SERVER",      FALSE,
                         "DIRECTORY-PATH",      FALSE,
                       "UPDATE-FREQUENCY",      FALSE,
                                "TABLEID",      FALSE,
                               "SYNONYMS",      FALSE,
                          "DEFAULT-GRAPH",      FALSE,
                               "DECIMALS",       TRUE,
                           "SHOWDECIMALS",      FALSE,
                               "ROUNDING",      FALSE,
                                 "MATRIX",       TRUE,
                          "AGGREGALLOWED",      FALSE,
                                "AUTOPEN",      FALSE,
                           "SUBJECT-CODE",       TRUE,
                           "SUBJECT-AREA",       TRUE,
                           "CONFIDENTIAL",      FALSE,
                              "COPYRIGHT",      FALSE,
                            "DESCRIPTION",       TRUE,
                                  "TITLE",       TRUE,
                     "DESCRIPTIONDEFAULT",      FALSE,
                               "CONTENTS",       TRUE,
                                  "UNITS",       TRUE,
                                   "STUB",       TRUE,
                                "HEADING",       TRUE,
                           "CONTVARIABLE",      FALSE,
                                 "VALUES",       TRUE,
                                "TIMEVAL",      FALSE,
                                  "CODES",      FALSE,
                           "DOUBLECOLUMN",      FALSE,
                               "PRESTEXT",      FALSE,
                                 "DOMAIN",      FALSE,
                          "VARIABLE-TYPE",      FALSE,
                            "HIERARCHIES",      FALSE,
                        "HIERARCHYLEVELS",      FALSE,
                    "HIERARCHYLEVELSOPEN",      FALSE,
                         "HIERARCHYNAMES",      FALSE,
                                    "MAP",      FALSE,
                            "PARTITIONED",      FALSE,
                            "ELIMINATION",      FALSE,
                              "PRECISION",      FALSE,
                           "LAST-UPDATED",      FALSE,
                                "STOCKFA",      FALSE,
                               "CFPRICES",      FALSE,
                                 "DAYADJ",      FALSE,
                                "SEASADJ",      FALSE,
                                "CONTACT",      FALSE,
                              "REFPERIOD",      FALSE,
                             "BASEPERIOD",      FALSE,
                               "DATABASE",      FALSE,
                                 "SOURCE",      FALSE,
                                 "SURVEY",      FALSE,
                                   "LINK",      FALSE,
                               "INFOFILE",      FALSE,
                        "FIRST-PUBLISHED",      FALSE,
                                "META-ID",      FALSE,
                    "OFFICIAL-STATISTICS",      FALSE,
                                   "INFO",      FALSE,
                                  "NOTEX",      FALSE,
                                   "NOTE",      FALSE,
                             "VALUENOTEX",      FALSE,
                              "VALUENOTE",      FALSE,
                              "CELLNOTEX",      FALSE,
                               "CELLNOTE",      FALSE,
                            "DATASYMBOL1",      FALSE,
                            "DATASYMBOL2",      FALSE,
                            "DATASYMBOL3",      FALSE,
                            "DATASYMBOL4",      FALSE,
                            "DATASYMBOL5",      FALSE,
                            "DATASYMBOL6",      FALSE,
                          "DATASYMBOLSUM",      FALSE,
                          "DATASYMBOLNIL",      FALSE,
                           "DATANOTECELL",      FALSE,
                            "DATANOTESUM",      FALSE,
                               "DATANOTE",      FALSE,
                                   "KEYS",      FALSE,
                           "ATTRIBUTE-ID",      FALSE,
                         "ATTRIBUTE-TEXT",      FALSE,
                             "ATTRIBUTES",      FALSE,
                                   "DATA",      TRUE
                    ) %>%
    dplyr::mutate(order = dplyr::row_number())
}
