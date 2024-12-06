smallest_larger_value <- function(vec, value) {
  min(vec[vec > value])
}

value_set_interval <- function(lines, head_line) {
  breaks <-
    c(stringr::str_which(lines, '^[:space:]*$'), length(lines) + 1)

  (head_line+1):(smallest_larger_value(breaks, head_line)-1)
}

get_chunk <- function(lines, head_line) {
  lines[value_set_interval(lines, head_line)]
}

extract_values <- function(lines) {
  lines %>%
    stringr::str_match("=(.*)$") %>%
    magrittr::extract(, 2)
}

extract_chunk <- function(lines, heading, key = NULL) {
  head_line <- stringr::str_which(lines, stringr::fixed(heading))

  chunk <- get_chunk(lines, head_line)

  if (! is.null(key)) {
     chunk <-  stringr::str_subset(chunk, stringr::str_glue("^{key}=.*$"))
  }

  extract_values(chunk)
}

new_classification <- function(name, prestext, domain, df) {
  vs <- list(name     = as.character(name),
             prestext = as.character(prestext),
             domain   = as.character(domain), #list
             # type     = "V", only support V
             df = df
             )

  structure(vs, class = "classification")
}

df_from_agg <- function(path) {
  agg_lines <- readLines_guess_encoding(path)

  aggregation_name <-
    extract_chunk(agg_lines, '[Aggreg]', 'Name') %>%
    stringr::str_trim()

  aggregation_levels <-
    extract_chunk(agg_lines, '[Aggtext]')

  df <- dplyr::tibble(valuecode           = as.character(),
                      !!aggregation_name := factor(levels = aggregation_levels,
                                                   ordered = TRUE
                                                   )
                      )

  for (aggregation_level in aggregation_levels) {
    df <- dplyr::bind_rows(df,
                           dplyr::tibble(valuecode = extract_chunk(agg_lines,
                                                                   paste0("[", aggregation_level, "]")
                                                                   ),
                                         !!aggregation_name := factor(aggregation_level,
                                                                      levels = aggregation_levels,
                                                                      ordered = TRUE
                                                                      )
                                         )
                           )
  }

  return(df)
}

px_classification_from_path <- function(vs_path, agg_paths = NULL) {
  vs_lines  <- readLines_guess_encoding(vs_path)

  vs_df <-
    dplyr::tibble(valuecode = extract_chunk(vs_lines, '[Valuecode]'),
                  valuetext = extract_chunk(vs_lines, '[Valuetext]')
                  )

  if (length(agg_paths) == 0) {
    df <- vs_df
  } else {
    agg_df <-
      agg_paths %>%
      purrr::map(df_from_agg) %>%
      purrr::reduce(dplyr::full_join, by = "valuecode")

    df <- dplyr::full_join(vs_df, agg_df, by = 'valuecode')
  }

  new_classification(name     = extract_chunk(vs_lines, '[Descr]', 'Name'),
                     prestext = extract_chunk(vs_lines, '[Descr]', 'Prestext'),
                     domain   = extract_chunk(vs_lines, '[Domain]'),
                     df = df
                     )
}

px_classification_from_df <- function(name, prestext, domain, df) {
  new_classification(name     = name,
                     prestext = prestext,
                     domain   = domain,
                     df       = df
                     )
}


#' Create a classification object
#'
#' Create a classification object from a data frame or .vs and .agg files.
#'
#' A classification is a combination of a value set and zero, one or more
#' aggregations. The classification can be saved as .vs and .agg files
#' (see [px_save_classification()]).
#'
#' If a classification is created from a data frame, the arguments 'name' and
#' 'domain' are required. If a classification is created from .vs and .agg files,
#' all other arguments should be empty.
#'
#' Type 'V' classifications are supported. Type 'H' and 'N' classifications are
#' not supported.
#'
#' @param name Optional. Name of the classification.
#' @param prestext Optional. Presentation text.
#' @param domain Optional. Domain name used to link to px-file.
#' @param df Optional. A data frame with required column 'valuecode' and
#' optional column 'valuetext', if the codes have texts. Each additional column
#' represents an aggregation. The column name is the name of the aggregation.
#' @param vs_path Optional. Path to a values set (.vs) file.
#' @param agg_paths Optional. Paths to one or more aggregation (.agg) files.
#'
#' @return A classification object
#'
#' @examples
#' # Create classification from data frame
#'
#' library(tibble)
#'
#' c1 <- px_classification(name = "Age5",
#'                         pretext = "Ages 0-9 - 60+",
#'                         domain = "age",
#'                         df = tribble(
#'                            ~valuecode,    ~valuetext,   ~`25 years classes`,
#'                                 "0-4",     "0-4 years",              "0-24",
#'                                 "5-9",     "5-9 years",              "0-24",
#'                               "10-14",   "10-14 years",              "0-24",
#'                               "15-19",   "15-19 years",              "0-24",
#'                               "20-24",   "20-24 years",              "0-24",
#'                               "25-29",   "25-29 years",             "25-49",
#'                               "30-34",   "30-34 years",             "25-49",
#'                               "35-39",   "35-39 years",             "25-49",
#'                               "40-44",   "40-44 years",             "25-49",
#'                               "45-49",   "45-49 years",             "25-49",
#'                               "50-54",   "50-54 years",             "50-74",
#'                               "55-59",   "55-59 years",             "50-74",
#'                               "60-64",   "60-64 years",             "50-74",
#'                               "65-69",   "65-69 years",             "50-74",
#'                               "70-74",   "70-74 years",             "50-74",
#'                                 "75+",     "75+ years",               "75+"
#'                                 )
#'                         )
#'
#' \dontrun{
#' # Create classification from .vs and .agg files
#'
#' c2 <- px_classification(vs_path = "path/to/value_set.vs",
#'                        agg_paths = c("path/to/aggregation1.agg",
#'                                      "path/to/aggregation2.agg"
#'                                      )
#'                        )
#' }
#'
#' @export
px_classification <- function(name, prestext="", domain="", df, vs_path=NULL, agg_paths=NULL) {
  validate_px_classification_arguments(name, prestext, domain, df, vs_path, agg_paths)

  if (all(is.null(vs_path), length(agg_paths) == 0)) {
    c <- px_classification_from_df(name, prestext, domain, df)
  } else {
    c <- px_classification_from_path(vs_path, agg_paths)
  }

  return(c)
}

enumerate_lines <- function(lines) {
  stringr::str_glue("{seq_along(lines)}={lines}") %>%
    paste(collapse = "\n")
}

blank_line <- function() " "

write_value_set <- function(c, directory) {
  filename <- file.path(directory, paste0(c$name, ".vs"))

  aggregation_names <-
    c$df %>%
    dplyr::select(-valuecode, -valuetext) %>%
    names() %>%
    paste0(".agg") %>%
    enumerate_lines()

  value_codes <-
    c$df %>%
    dplyr::pull(valuecode) %>%
    enumerate_lines()

  value_text <-
    c$df %>%
    dplyr::pull(valuetext) %>%
    enumerate_lines()

  vs_lines <-
    stringr::str_glue(
    "[Descr]",
    "Name={c$name}",
    "Prestext={c$prestext}",
    "Type=V",
    blank_line(),
    "[Aggreg]",
    aggregation_names,
    blank_line(),
    "[Domain]",
    enumerate_lines(c$domain),
    blank_line(),
    "[Valuecode]",
    value_codes,
    blank_line(),
    "[Valuetext]",
    value_text,
    .sep = "\n"
    )

  file_connection <- file(filename, encoding = "ISO-8859-1")
  writeLines(vs_lines, file_connection)
  close(file_connection)
}

write_aggregation <- function(aggregation, c, path) {
  filename <- file.path(path, paste0(aggregation, ".agg"))

  groups <- levels(c$df[[aggregation]])

  agg_texts <-
    c$df %>%
    dplyr::distinct(valuecode, !!rlang::sym(aggregation)) %>%
    dplyr::arrange(as.character(!!rlang::sym(aggregation))) %>%
    tidyr::pivot_wider(names_from = all_of(aggregation),
                       values_from  = "valuecode",
                       values_fn = list
                       ) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(str =
                    stringr::str_glue("[{name}]\n",
                                      "{paste(enumerate_lines(value), collapse = '\n')}"
                    )
    ) %>%
    dplyr::pull(str) %>%
    paste0(collapse = paste0("\n", blank_line(), "\n"))

  agg_lines <-
    stringr::str_glue(
      "[Aggreg]",
      "Name={aggregation}",
      "Valueset={c$name}",
      enumerate_lines(groups),
      blank_line(),
      "[Aggtext]",
      enumerate_lines(groups),
      blank_line(),
      agg_texts,
      .sep = "\n"
    )

  file_connection <- file(filename, encoding = "ISO-8859-1")
  writeLines(agg_lines, file_connection)
  close(file_connection)
}

#' Save classification as .vs and .agg files
#'
#' Save a classification object as .vs and .agg files. The .vs file contains
#' the value set and the .agg files contain the aggregations.
#'
#' @param c A classification object
#' @param path Directory to save the files in
#'
#' @returns Nothing
#'
#' @examples
#' c <- px_classification(name = "Age5",
#'                        pretext = "Ages 0-9 - 60+",
#'                        domain = "age",
#'                        df = classification_age
#'                        )
#'
#' px_save_classification(c, path = tempdir())
#' @export
px_save_classification <- function(c, path) {
  write_value_set(c, path)

  aggregations <-
    c$df %>%
    dplyr::select(-valuecode, -valuetext) %>%
    names()

  purrr::walk(aggregations, write_aggregation, c = c, path = path)
}
