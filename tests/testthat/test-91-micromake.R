test_that("micromake runs without errors and creates px files", {
  test_file_creation2 <- function(data_path) {
    micro_metadata <- temp_xlsx_file()

    df <-
      readRDS(data_path) %>%
      dplyr::as_tibble() %>%
      dplyr::select(taar, civst, civdto, kirke)

    make_template(data_df = df,
                  languages = c("en"),
                  out_path = micro_metadata
                  )

    temp_dir <- temp_dir()

    expect_warning(micromake(data_df = df,
                             metadata_path = micro_metadata,
                             out_dir = temp_dir
                             ),
                   regexp = NA
                   )

    expect_true(length(list.files(temp_dir)) > 0, not(gives_warning()))
  }

  test_file_creation2(get_data_path("micro"))
})

# test_that("micromake creates valid px files", {
#   skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")
#
#   # expect_that_pxjob_runs_without_errors <- function(table_name) {
#   #   rds <- pxmake_clean(input = get_metadata_path(table_name),
#   #                       data  = get_data_path(table_name)
#   #                       )
#   #
#   #   meta <- temp_xlsx_file()
#   #
#   #   metamake_clean(input = rds,
#   #                  out_path = meta,
#   #                  create_data = FALSE
#   #                  )
#   #
#   #   temp_dir <- temp_dir()
#   #
#   #   micromake(data_df = rds$data,
#   #             metadata_path = meta,
#   #             out_dir = temp_dir
#   #             )
#   #
#   #   px_paths <- list.files(temp_dir, full.names = TRUE)
#   #
#   #   for (px_path in px_paths) {
#   #     output <- temp_px_file()
#   #
#   #     pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
#   #     expect_equal(0, pxjob_exit_code)
#   #   }
#   # }
#
#   expect_that_pxjob_runs_without_errors <- function(table_name) {
#     rds <- pxmake_clean(input = get_metadata_path(table_name),
#                         data  = get_data_path(table_name)
#     )
#
#     meta <- temp_xlsx_file()
#
#     metamake_clean(input = rds,
#                    out_path = meta,
#                    create_data = FALSE
#     )
#
#     temp_dir <- temp_dir()
#
#     micromake(data_df = rds$data,
#               metadata_path = meta,
#               out_dir = temp_dir
#     )
#
#     px_paths <- list.files(temp_dir, full.names = TRUE)
#
#     for (px_path in px_paths) {
#       output <- temp_px_file()
#
#       pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
#       expect_equal(0, pxjob_exit_code)
#     }
#   }
#
#   expect_that_pxjob_runs_without_errors(get_data_path("micro"))
#   expect_that_pxjob_runs_without_errors("BEXLTALL")
#   expect_that_pxjob_runs_without_errors("FOTEST")
#   expect_that_pxjob_runs_without_errors("no_timeval_or_codes")
#   expect_that_pxjob_runs_without_errors("zero_heading")
#   expect_that_pxjob_runs_without_errors("zero_stub")
# })
