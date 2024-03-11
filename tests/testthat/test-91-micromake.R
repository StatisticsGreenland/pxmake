test_that("micromake creates px files correctly", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_micro_files_are_correct <- function(x) {
    n_microvars <- length(names(x$data)) - length(heading(x))

    out_dir <- temp_dir()

    micromake(x, out_dir = out_dir)

    px_paths <- list.files(out_dir, full.names = TRUE)

    expect_identical(n_microvars, length(px_paths))

    for (px_path in px_paths) {
      x_micro <- px(px_path)
      micro_var <- stub(x_micro)

      expect_true(stringr::str_detect(px_path, micro_var))

      expect_identical(heading(x), heading(x_micro))

      expect_identical(figures(x_micro), "n")

      expect_identical(note(x_micro),
                       list("Table note",
                            dplyr::tibble(`variable-code` = micro_var,
                                          note = paste0("note for ", micro_var)
                            )
                       )
      )
    }
  }

  get_data_path("micro") %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::select(taar, sex, civst, alder) %>%
    dplyr::mutate(alder = cut(alder, breaks = c(0, 20, 40, 60, 80, 100),
                              labels = c("0-19", "20-39", "40-59", "60-79", "80+"))
                  ) %>%
    px() %>%
    stub("civst") %>%
    timeval("taar") %>%
    heading(c("taar", "sex")) %>%
    note("Table note") %>%
    note(dplyr::tibble(`variable-code` = c("civst", "alder"),
                       note = paste0("note for ", `variable-code`)
                       )
         ) %>%
    expect_that_micro_files_are_correct()
})

test_that("micromake creates valid px files", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  expect_that_pxjob_runs_without_errors <- function(px) {
    out_dir <- temp_dir()

    micromake(px, out_dir = out_dir)

    px_paths <- list.files(out_dir, full.names = TRUE)

    for (px_path in px_paths) {
      output <- temp_px_file()
      pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)

      expect_equal(0, pxjob_exit_code, info = px_path)
    }
  }

  get_data_path("micro") %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(sidedoer = stringr::str_trim(sidedoer),
                  sidedoer = dplyr::na_if(sidedoer, ""),
                  pnr = NA
                  ) %>%
    px() %>%
    timeval("taar") %>%
    expect_that_pxjob_runs_without_errors()
})

test_that("micromake can control data for individual tables", {
  set.seed(1)

  df <-
    get_data_path("micro") %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::select(taar, civst) %>%
    dplyr::mutate(study = sample(c("A", "B"), size = nrow(.), replace = TRUE))

  table_level <-
    dplyr::tribble(~variable, ~description,
                   "taar", "Year",
                   "civst", "Civil status"
                   ) %>%
    dplyr::mutate(matrix = variable)

  out_dir <- temp_dir()

  px(df) %>%
    stub(names(df)) %>%
    heading("study") %>%
    micromake(out_dir = out_dir,
              keyword_values = table_level
              )

  px_paths <- list.files(out_dir, full.names = TRUE)

  for (px_path in px_paths) {
    x_micro <- px(px_path)
    micro_var <- stub(x_micro)

    micro_table_level <-
      table_level %>%
      dplyr::filter(variable == micro_var)

    expect_equal(description(x_micro), micro_table_level$description)
    expect_equal(matrix(x_micro), micro_table_level$matrix)
  }
})

test_that("micromake can control filenames", {
  df <-
    get_data_path("micro") %>%
    readRDS() %>%
    dplyr::as_tibble() %>%
    dplyr::select(1:4)

  out_dir <- temp_dir()

  filename_df <-
    dplyr::tibble(variable = names(df),
                  filename = paste0("micro_", variable, ".px")
                  ) %>%
    dplyr::arrange(variable) %>%
    head(2)

  px(df) %>%
    stub(names(df)) %>%
    micromake(out_dir = out_dir, keyword_values = filename_df)

  expect_equal(list.files(out_dir),
               c(filename_df$filename, "pnrmor.px", "taar.px")
               )
})
