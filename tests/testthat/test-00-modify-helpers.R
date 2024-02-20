test_that("Value is added/modified", {
  df1 <- data.frame(a = c(1, 2, 1),
                    b = c(4, 5, 6)
                    )

  df2 <- data.frame(a = c(1, 2, 1),
                    b = c(4, 7, 6)
                    )

  expect_equal(modify_or_add_row(df1,
                                 lookup_column = "a", lookup_column_values = 2,
                                 modify_column = "b", new_value = 7
                                 ),
               df2
               )

  df3 <- data.frame(a = c(1, 2, 1),
                    b = c(8, 5, 8)
                    )

  expect_equal(modify_or_add_row(df1,
                                 lookup_column = "a", lookup_column_values = 1,
                                 modify_column = "b", new_value = 8
                                 ),
               df3
               )

  df4 <- data.frame(a = c(1, 2, 1),
                    b = c(8, 8, 8)
                    )

  expect_equal(modify_or_add_row(df1,
                                 lookup_column = "a", lookup_column_values = 1:2,
                                 modify_column = "b", new_value = 8
                                 ),
               df4
               )

  df5 <- data.frame(a = c(1, 2, 1),
                    b = c(4, 5, 6)
                    )

  df6 <- data.frame(a = c(7, 2, 7),
                    b = c(4, 5, 6)
                    )


  expect_equal(modify_or_add_in_column(df5,
                                       lookup_column = "a",
                                       lookup_column_values = 1,
                                       new_value = 7
                                       ),
               df6
               )

  df7 <- data.frame(a = c(1, 2, 1, 7),
                    b = c(4, 5, 6, NA)
                    )

  expect_equal(modify_or_add_in_column(df5,
                                       lookup_column = "a",
                                       lookup_column_values = 4,
                                       new_value = 7
                                       ),
               df7
               )

})

test_that("Data frame modifier works", {
  arrange_and_expect_equal <- function(df1, df2) {
    expect_identical(dplyr::arrange(df1, dplyr::across(everything())),
                     dplyr::arrange(df2, dplyr::across(everything()))
                     )
  }

  df1 <- data.frame(a = c(1, 1, 2),
                    b = c(3, 4, 3),
                    c = c(7, 8, 9)
                    )

  df2 <- data.frame(a = c(1, 1),
                    b = c(3, 4),
                    c = c(1, 2)
                    )

  df3 <- data.frame(a = c(1, 1, 2),
                    b = c(3, 4, 3),
                    c = c(1, 2, 9)
                    )

  arrange_and_expect_equal(modify_with_df(df1, df2, "c"), df3)

  df4 <- data.frame(a = c(3),
                    b = c(1),
                    c = c(5)
                    )

  df5 <- data.frame(a = c(1, 1, 2, 3),
                    b = c(3, 4, 3, 1),
                    c = c(7, 8, 9, 5)
                    )

  arrange_and_expect_equal(modify_with_df(df1, df4, "c"), df5)

  df6 <- data.frame(a = c(1, 1, 3),
                    b = c(3, 5, 3),
                    c = c(6, 7, 9)
                    )

  df7 <- data.frame(a = c(1, 1, 2, 3, 1),
                    b = c(3, 4, 3, 3, 5),
                    c = c(6, 8, 9, 9, 7)
                    )

  arrange_and_expect_equal(modify_with_df(df1, df6, "c"), df7)

  df8 <- data.frame(a = c(1, 2, 3),
                    c = c(5, 6, 7)
                    )

  df9 <- data.frame(a = c(1, 1, 2, 3),
                    b = c(3, 4, 3, NA),
                    c = c(5, 5, 6, 7)
                    )

  arrange_and_expect_equal(modify_with_df(df1, df8, "c"), df9)

  df10 <- data.frame(d = c(1, 1, 2, 3),
                     e = c(3, 4, 3, 3),
                     c = c(7, 8, 9, 9)
                     )

  expect_error(modify_with_df(df1, df10, "c"), regexp = "invalid columns: d, e")
})
