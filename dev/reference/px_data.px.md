# DATA

Inspect or change DATA.

## Usage

``` r
px_data(x, value, labels, validate)

# S3 method for class 'px'
px_data(x, value, labels = FALSE, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame. If missing, the current DATA is returned. If
  NULL, all data rows are removed.

- labels:

  Optional. Logic or character vector. If TRUE, the data table is
  returned with VALUES instead of CODES. By default the VALUES of the
  main language are returned, use a character language code to return
  VALUES for a specific language.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/dev/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or data frame.

## Details

If adding a new data frame, metadata is generated for the new columns
and removed for columns that are no longer present.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A32%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C502%2C0%5D)

## Examples

``` r
x1 <- px(population_gl)

# Print data table
px_data(x1)
#> # A tibble: 30 × 4
#>    gender age   year      n
#>    <fct>  <fct> <fct> <dbl>
#>  1 female 0-6   2004   3109
#>  2 female 0-6   2014   2644
#>  3 female 0-6   2024   2668
#>  4 female 17-24 2004   3003
#>  5 female 17-24 2014   3528
#>  6 female 17-24 2024   2862
#>  7 female 25-64 2004  13744
#>  8 female 25-64 2014  14397
#>  9 female 25-64 2024  15098
#> 10 female 65+   2004   1630
#> # ℹ 20 more rows

# Change data table
population_gl_2024 <- subset(population_gl, year == 2024)

x2 <- px_data(x1, population_gl_2024)

# Return data table with VALUES instead of CODES
px_data(x1, labels = TRUE)
#> # A tibble: 30 × 4
#>    gender age   year      n
#>    <fct>  <fct> <fct> <dbl>
#>  1 female 0-6   2004   3109
#>  2 female 0-6   2014   2644
#>  3 female 0-6   2024   2668
#>  4 female 17-24 2004   3003
#>  5 female 17-24 2014   3528
#>  6 female 17-24 2024   2862
#>  7 female 25-64 2004  13744
#>  8 female 25-64 2014  14397
#>  9 female 25-64 2024  15098
#> 10 female 65+   2004   1630
#> # ℹ 20 more rows

# Return VALUES for a specific language
x_mult <-
  x1 |>
  px_languages(c("en", "gl"))

px_data(x_mult, labels = "gl")
#> # A tibble: 30 × 4
#>    gender age   year      n
#>    <fct>  <fct> <fct> <dbl>
#>  1 female 0-6   2004   3109
#>  2 female 0-6   2014   2644
#>  3 female 0-6   2024   2668
#>  4 female 17-24 2004   3003
#>  5 female 17-24 2014   3528
#>  6 female 17-24 2024   2862
#>  7 female 25-64 2004  13744
#>  8 female 25-64 2014  14397
#>  9 female 25-64 2024  15098
#> 10 female 65+   2004   1630
#> # ℹ 20 more rows
```
