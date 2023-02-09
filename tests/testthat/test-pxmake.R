get_source_data_path <- function(table_name) {
  test_path('fixtures', 'data', paste0(table_name, '.rds'))
}

get_metadata_path <- function(table_name) {
  test_path('fixtures', 'metadata', glue::glue("metadata_{table_name}.xlsx"))
}

get_pxfile_path <- function(table_name) {
  test_path('px', paste0(table_name, '.px'))
}

get_pxjobfile_path <- function(table_name) {
  test_path('px', paste0(table_name, '_pxjob.px'))
}


test_that("pxmake runs without errors and creates a file", {
  test_file_creation <- function(table_name) {
    if (file.exists(get_pxfile_path(table_name))) {
      file.remove(get_pxfile_path(table_name))
    }

    pxmake(get_source_data_path(table_name),
           get_metadata_path(table_name),
           get_pxfile_path(table_name)
           )

    expect_true(file.exists(get_pxfile_path(table_name)))
  }

  test_file_creation("BEXLTALL")
  test_file_creation("BEXSTA_large")
  test_file_creation("BEXSTA_small")
  test_file_creation("FOTEST")
})

test_that("pxjob exists without errors (exit code 0)", {
  skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")

  run_pxjob <- function(table_name) {
    pxjob64Win::pxjob(input = get_pxfile_path(table_name),
                      output = get_pxjobfile_path(table_name)
                      )
  }

  expect_equal(run_pxjob("BEXLTALL"), 0)
  expect_equal(run_pxjob("BEXSTA_large"), 0)
  expect_equal(run_pxjob("BEXSTA_small"), 0)
  expect_equal(run_pxjob("FOTEST"), 0)
})
