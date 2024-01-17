# micromake_wrapper <- function(data_df, out_dir) {
#   micro_metadata <- temp_xlsx_file()
#
#   make_template(data_df = data_df,
#                 languages = c("en"),
#                 time_var= "taar",
#                 out_path = micro_metadata,
#                 figures_variable = "n"
#                 )
#
#   micromake(data_df = data_df,
#             metadata_path = micro_metadata,
#             out_dir = out_dir
#             )
# }
#
# test_that("micromake creates valid px files", {
#   skip_if_not_installed("pxjob64Win", minimum_version = "1.1.0")
#
#   expect_that_pxjob_runs_without_errors <- function(data_df) {
#     out_dir <- temp_dir()
#
#     micromake_wrapper(data_df = data_df,
#                       out_dir = out_dir
#                       )
#
#     px_paths <- list.files(out_dir, full.names = TRUE)
#
#     for (px_path in px_paths) {
#       output <- temp_px_file()
#       pxjob_exit_code <- pxjob64Win::pxjob(px_path, output)
#       expect_equal(0, pxjob_exit_code)
#     }
#   }
#
#   get_data_path("micro") %>%
#     readRDS() %>%
#     dplyr::as_tibble() %>%
#     dplyr::mutate(sidedoer = stringr::str_trim(sidedoer),
#                   sidedoer = dplyr::na_if(sidedoer, ""),
#                   pnr = NA
#                   ) %>%
#       expect_that_pxjob_runs_without_errors()
# })
