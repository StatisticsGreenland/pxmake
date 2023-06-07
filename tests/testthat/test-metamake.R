test_that("file encoding is correct", {
  get_file_encoding_for_table <- function(table_name) {
    get_encoding_from_px_file(get_pxfile_path(table_name))
  }

  expect_equal(get_file_encoding_for_table('TUX01'),   'iso-8859-15')
  expect_equal(get_file_encoding_for_table('BEXSTA_windows_1252'), 'Windows-1252')

  # no encoding listed; utf-8 is default
  pxmake_clean(get_metadata_path("BEXSTA"),
               get_pxfile_path("BEXSTA"),
               get_source_data_path("BEXSTA")
               )
  expect_equal(get_file_encoding_for_table('BEXSTA'),  'utf-8')
})

test_that("pxfile = pxmake(metamake(pxfile))", {
  run_metamake_pxmake_and_compare <- function(table_name) {
    px1   <- get_pxfile_path(table_name)
    meta1 <- get_metadata_path(table_name)
    data1 <- get_source_data_path(table_name)
    px2   <- get_pxfile_path(paste0(table_name, "_metamake_pxmake"))
    meta2 <- get_metadata_path(paste0(table_name, "_by_metamake"))

    pxmake_clean(meta1, px1, data1)
    metamake_clean(px1, meta2)
    pxmake_clean(meta2, px2)

    expect_equal_lines(px1, px2)
  }

  run_metamake_pxmake_and_compare("BEXSTA")
  run_metamake_pxmake_and_compare("FOTEST")
  run_metamake_pxmake_and_compare("BEXLTALL")
  run_metamake_pxmake_and_compare("no_timeval_or_codes")
})

test_that("metamake can create rds file", {
  table_name   <- 'BEXSTA'
  px1     <- get_pxfile_path(table_name)
  meta1   <- get_metadata_path(table_name)
  data1   <- get_source_data_path(table_name)

  table_name2  <- paste0(table_name, "_by_metamake")
  rds_out <- get_source_data_path(table_name2)
  meta2   <- get_metadata_path(table_name2)
  px2     <- get_pxfile_path(table_name2)

  pxmake_clean(meta1, px1, data1)
  metamake_clean(px1, meta2, rds_data_path = rds_out)
  pxmake_clean(meta2, px2, rds_out)

  expect_true(file.exists(rds_out))
  expect_equal_lines(px1, px2)
})

test_that("pxfile and pxmake(metamake(pxfile)) are equivalent", {
  # Some files don't give an exact match in the px-file because there can be
  # subtle differences in formatting. PxJob is run on these file to test
  # that they are equivalent.
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_metamake_pxmake_pxjob_and_compare <- function(table_name) {
    px1   <- get_pxfile_path(table_name)

    table_name2  <- paste0(table_name, "_by_metamake")
    px2   <- get_pxfile_path(paste0(table_name, "_metamake_pxmake"))
    meta2 <- get_metadata_path(paste0(table_name, "_by_metamake"))

    pxjob1 <- get_pxjobfile_path(table_name)
    pxjob2 <- get_pxjobfile_path(table_name2)

    metamake_clean(px1, meta2)
    pxmake_clean(meta2, px2)

    pxjob_clean(input = px1, output = pxjob1)
    pxjob_clean(input = px2, output = pxjob2)

    #' Read pxjob file and remove lines that doesn't need to be equal
    read_pxjobfile <- function(path) {
      lines <-
        path %>%
        readLines() %>%
        stringr::str_subset("^VARIABLECODE.+", negate = TRUE)

      if (table_name %in% c("no_timeval_or_codes2")) {
        lines <- stringr::str_subset(lines, "^CODES.+", negate = TRUE)
      }

      return(lines)
    }

    expect_equal(read_pxjobfile(pxjob1), read_pxjobfile(pxjob2))
  }

  run_metamake_pxmake_pxjob_and_compare("SOXATI4")
  run_metamake_pxmake_pxjob_and_compare("BEXSTA_windows_1252")
  run_metamake_pxmake_pxjob_and_compare("no_timeval_or_codes2")

  # Turn on when support for CELLNOTEX is added (issue #101)
  # run_metamake_pxmake_pxjob_and_compare("TUX01")
})
