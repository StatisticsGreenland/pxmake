test_that("px file is created", {
  test_file_creation <- function(source_data_path,
                                 metadata_path,
                                 pxfile_path) {
    if (file.exists(pxfile_path)) {
      file.remove(pxfile_path)
    }

    pxmake(source_data_path, metadata_path, pxfile_path)

    expect_true(file.exists(pxfile_path))
  }

  test_file_creation(source_data_path = test_path('fixtures', 'data', 'BEXLTALL.rds'),
                     metadata_path = test_path('fixtures', 'metadata', 'metadata_BEXLTALL.xlsx'),
                     pxfile_path = test_path('px', 'BEXLTAL.px')
                     )

  test_file_creation(source_data_path = test_path('fixtures', 'data', 'BEXSTA_large.rds'),
                     metadata_path = test_path('fixtures', 'metadata', 'metadata_BEXSTA_large.xlsx'),
                     pxfile_path = test_path('px', 'BEXSTA_large.px')
  )

  test_file_creation(source_data_path = test_path('fixtures', 'data', 'BEXSTA_small.rds'),
                     metadata_path = test_path('fixtures', 'metadata', 'metadata_BEXSTA_small.xlsx'),
                     pxfile_path = test_path('px', 'BEXSTA_small.px')
  )

  test_file_creation(source_data_path = test_path('fixtures', 'data', 'FOTEST.rds'),
                     metadata_path = test_path('fixtures', 'metadata', 'metadata_FOTEST.xlsx'),
                     pxfile_path = test_path('px', 'FOTEST.px')
  )
})
