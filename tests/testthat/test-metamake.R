run_metamake_and_pxmake <- function(table_name) {
  px_source    <- get_pxfile_path(table_name)
  metadata_out <- get_metadata_path(paste0(table_name, "_by_metamake"))
  px_out       <- get_pxfile_path(paste0(table_name, "_metamake_pxmake"))

  metamake(px_source, metadata_out)

  pxmake(metadata_out, px_out)
}

test_that("file encoding is correct", {
  get_file_encoding_for_table <- function(table_name) {
    get_pxfile_encoding(get_pxfile_path(table_name))
  }

  expect_equal(get_file_encoding_for_table('TUX01'),   'iso-8859-15')
  expect_equal(get_file_encoding_for_table('BEXSTA_windows_1252'), 'Windows-1252')

  # no encoding listed; utf-8 is default
  expect_equal(get_file_encoding_for_table('BEXSTA'),  'utf-8')
})

test_that("pxfile = pxmake(metamake(pxfile))", {
  run_metamake_pxmake_and_compare <- function(table_name) {
    run_metamake_and_pxmake(table_name)

    output <- readLines(get_pxfile_path(table_name))
    expect <- readLines(get_pxfile_path(paste0(table_name, "_metamake_pxmake")))

    expect_equal(output, expect)
  }

  run_metamake_pxmake_and_compare("BEXSTA")
  run_metamake_pxmake_and_compare("FOTEST")
  run_metamake_pxmake_and_compare("BEXLTALL")
  run_metamake_pxmake_and_compare("no_timeval")
})

test_that("metamake can create rds file", {
  table_name   <- 'BEXSTA'
  px_source    <- get_pxfile_path(table_name)
  metadata_out <- get_metadata_path(paste0(table_name, "_by_metamake"))
  rds_out      <- tempfile(fileext = ".rds")
  px_out       <- get_pxfile_path(paste0(table_name, "_metamake_pxmake_rds"))

  metamake(px_source, metadata_out, rds_data_path = rds_out)
  pxmake(metadata_out, px_out, rds_out)

  output <- readLines(get_pxfile_path(paste0(table_name, "_metamake_pxmake")))
  expect <- readLines(get_pxfile_path(paste0(table_name, "_metamake_pxmake_rds")))

  expect_equal(output, expect)
})

test_that("pxfile and pxmake(metamake(pxfile)) are equivalent", {
  # Some files don't give an exact match in the px-file because there can be
  # subtle differences in formatting. PxJob is run on these file to test
  # that they are equivalent.
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_metamake_pxmake_pxjob_and_compare <- function(table_name) {
    run_metamake_and_pxmake(table_name)

    pxjob64Win::pxjob(input = get_pxfile_path(table_name),
                      output = get_pxjobfile_path(table_name)
    )

    pxjob64Win::pxjob(input = get_pxfile_path(paste0(table_name, "_metamake_pxmake")),
                      output = get_pxjobfile_path(paste0(table_name, "_metamake_pxmake"))
    )

    output <- readLines(get_pxjobfile_path(table_name))
    expect <- readLines(get_pxjobfile_path(paste0(table_name, "_metamake_pxmake")))

    expect_equal(output, expect)
  }

  run_metamake_pxmake_pxjob_and_compare("SOXATI4")

  # Turn on when lines are broken correctly (issue #103)
  run_metamake_pxmake_pxjob_and_compare("BEXSTA_windows_1252")

  # Turn on when support for CELLNOTEX is added (issue #101)
  # run_metamake_pxmake_pxjob_and_compare("TUX01")
})
