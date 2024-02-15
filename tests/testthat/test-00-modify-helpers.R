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
