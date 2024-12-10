smallest_larger_value <- function(vec, value) {
  min(vec[vec > value])
}

chunk_interval <- function(lines, head_line) {
 breaks <-
    c(stringr::str_which(lines, '^\\[[^\\]]+\\]$'), length(lines)+1)

  (head_line+1):(smallest_larger_value(breaks, head_line)-1)
}

get_chunk <- function(lines, head_line) {
  lines[chunk_interval(lines, head_line)]
}

extract_chunk <- function(lines, heading, key = NULL) {
  head_lines <- stringr::str_which(lines, stringr::fixed(heading))

  if (identical(head_lines, integer(0))) {
    return(NULL)
  }

  colname <- stringr::str_remove_all(heading, "\\[|\\]") %>% tolower()

  chunk <-
    head_lines %>%
    purrr::map(~ get_chunk(lines, .x)) %>%
    unlist() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(across(value, stringr::str_trim)) %>%
    dplyr::filter(value != "") %>%
    tidyr::drop_na(value) %>%
    tidyr::separate_wider_delim(cols = value, delim = "=", names = c('id', colname))


  if (! is.null(key)) {
     chunk <- chunk %>% dplyr::filter(id == key)
  }

  return(chunk)
}

new_classification <- function(name, prestext, domain, df) {
  if (length(name) == 0 | length(prestext) == 0 | length(domain) == 0) {
    stop("name, prestext, and domain must be non-empty.")
  }

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
    basename(path) %>%
    stringr::str_remove("\\.agg$")

  aggregation_groups_df <-
    extract_chunk(agg_lines, '[Aggreg]') %>%
    dplyr::filter(stringr::str_detect(id, "^\\d+$"))

  aggregation_text_df <- extract_chunk(agg_lines, '[Aggtext]')

  if (is.null(aggregation_text_df)) {
    aggregation_df <- dplyr::select(aggregation_groups_df, -id)
  } else {
    if (nrow(aggregation_groups_df) != nrow(aggregation_text_df)) {
      warning(paste0("The number of aggregation groups (",
                     nrow(aggregation_groups_df), ") and Aggtexts (",
                     nrow(aggregation_text_df),
                     ") differ in '",  basename(path), "'."
                     )
              )
    }

    aggregation_df <-
      dplyr::left_join(aggregation_groups_df, aggregation_text_df, by = "id") %>%
      dplyr::select(-id) %>%
      dplyr::mutate(across(everything(), ~ dplyr::na_if(.x, "")))
  }

  df <- dplyr::tibble(valuecode           = as.character(),
                      !!aggregation_name := factor(levels = aggregation_df$aggreg,
                                                   ordered = TRUE
                                                   )
                      )

  for (aggregation_group in aggregation_df$aggreg) {
    aggregation_values <-
      extract_chunk(agg_lines, paste0("[", aggregation_group, "]")) %>%
      dplyr::select(-id) %>%
      dplyr::pull(1)

    df <-
      dplyr::bind_rows(df,
                       dplyr::tibble(valuecode = aggregation_values,
                                     !!aggregation_name := factor(aggregation_group,
                                                                  levels = aggregation_df$aggreg,
                                                                  ordered = TRUE
                                                                  )
                                     )
                           )
  }

  return(df)
}

px_classification_from_path <- function(vs_path, agg_paths) {
  vs_lines  <- readLines_guess_encoding(vs_path)

  valuecode_df <- extract_chunk(vs_lines, '[Valuecode]')
  valuetext_df <- extract_chunk(vs_lines, '[Valuetext]')

  if (is.null(valuetext_df)) {
    vs_df <- dplyr::select(valuecode_df, -id)
  } else {
    if (nrow(valuecode_df) != nrow(valuetext_df)) {
      warning("[Valuecode] and [Valuetext] have different number of rows.")
    }

    vs_df <-
      dplyr::left_join(valuecode_df, valuetext_df, by = "id") %>%
      dplyr::select(-id) %>%
      dplyr::mutate(across(everything(), ~ dplyr::na_if(.x, "")))
  }

  if (missing(agg_paths)) {
    vs_dir <- dirname(vs_path)

    agg_paths <- file.path(vs_dir, extract_chunk(vs_lines, '[Aggreg]')$aggreg)
  }

  if (any(!file.exists(agg_paths))) {
    warning(paste0("Aggregation files: ",
                   paste(basename(agg_paths[!file.exists(agg_paths)]),
                         collapse = ", "
                         ),
                   " do not exist."
                   )
            )

    agg_paths <- agg_paths[file.exists(agg_paths)]
  }

  if (length(agg_paths) == 0) {
    df <- vs_df
  } else {
    agg_df <-
      agg_paths %>%
      purrr::map(df_from_agg) %>%
      purrr::compact() %>%
      purrr::reduce(dplyr::full_join, by = "valuecode")

    df <- dplyr::full_join(vs_df, agg_df, by = 'valuecode')
  }

  new_classification(name     = extract_chunk(vs_lines, '[Descr]', 'Name') %>% dplyr::pull(2),
                     prestext = extract_chunk(vs_lines, '[Descr]', 'Prestext') %>% dplyr::pull(2),
                     domain   = extract_chunk(vs_lines, '[Domain]') %>% dplyr::pull(2),
                     df = df
                     )
}

px_classification_from_df <- function(name, prestext, domain, df) {
  character_columns <- intersect(names(df), c("valuecode", "valuetext"))

  df_formatted <-
    df %>%
    dplyr::mutate(across(all_of(character_columns), as.character),
                  across(-all_of(character_columns), ~ factor(.x, ordered = TRUE))
                  )

  new_classification(name     = name,
                     prestext = prestext,
                     domain   = domain,
                     df       = df_formatted
                     )
}


#' Create a classification object
#'
#' Create a classification object from a data frame or .vs and .agg files.
#'
#' A classification is a combination of a value set and zero, one, or more
#' aggregations. The classification can be saved as .vs and .agg files
#' (see [px_save_classification()]).
#'
#' If a classification is created from a data frame, the arguments 'name' and
#' 'domain' are required. If a classification is created from .vs and .agg files,
#' all other arguments should be empty.
#'
#' Type 'V' value sets are supported. Type 'H' and 'N' value set are
#' not supported.
#'
#' @param name Optional. Name of the classification.
#' @param prestext Optional. Presentation text.
#' @param domain Optional. Character vector with domain names. Used to link to
#' px-file.
#' @param df Optional. A data frame with required column 'valuecode' and
#' optional column 'valuetext', if the codes have texts. Each additional column
#' represents an aggregation. The column name is the name of the aggregation.
#' @param vs_path Optional. Path to a values set (.vs) file.
#' @param agg_paths Optional.
#' \itemize{
#'   \item If NULL, aggregation paths are automatically taken from the
#'   \[Aggreg\] field in the .vs file.
#'   \item Use a vector of paths to one or more aggregation files (.agg) to
#'   manually choose aggregations.
#'   \item Use character(0) if aggregations from the .vs files should not be
#'   added automatically.
#' }
#' @return A classification object
#'
#' @examples
#' # Create classification from data frame
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
#' # Create classification from .vs file and use aggregations mentioned in .vs
#' c2 <- px_classification(vs_path = "path/to/value_set.vs")
#'
#' # Create classification from .vs file and manually specify aggregation files
#' c3 <- px_classification(vs_path = "path/to/value_set.vs",
#'                        agg_paths = c("path/to/aggregation1.agg",
#'                                      "path/to/aggregation2.agg"
#'                                      )
#'                        )
#'
#'
#' }
#'
#' @export
px_classification <- function(name, prestext, domain, df, vs_path, agg_paths) {
  validate_px_classification_arguments(name, prestext, domain, df, vs_path, agg_paths)

  if (all(missing(vs_path), missing(agg_paths))) {
    c <- px_classification_from_df(name, prestext, domain, df)
  } else if (missing(agg_paths)) {
    c <- px_classification_from_path(vs_path)
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

  aggregation_df <-
    c$df %>%
    dplyr::select(-valuecode, -valuetext)

  if (ncol(aggregation_df) == 0) {
    aggregation_text <- ''
  } else {
    aggregation_names <-
      aggregation_df %>%
      names() %>%
      paste0(".agg") %>%
      enumerate_lines()

    aggregation_text <-
      stringr::str_glue("[Aggreg]",
                        aggregation_names,
                        "{blank_line()}\n",
                        .sep = "\n"
                        )
  }

  get_values <- function(var) {
    c$df %>%
      tidyr::replace_na(setNames(list(""), var)) %>%
      dplyr::pull(var) %>%
      enumerate_lines()
  }

  value_codes <- get_values("valuecode")
  value_text <- get_values("valuetext")

  vs_lines <-
    stringr::str_glue(
    "[Descr]",
    "Name={c$name}",
    "Prestext={c$prestext}",
    "Type=V",
    blank_line(),
    aggregation_text,
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
