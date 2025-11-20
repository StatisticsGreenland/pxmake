# Add total levels to variables

Adds a total level, which is the sum of the figures for all other levels
of the variable.

The default name of the total level is 'Total', unless
[px_elimination](https://statisticsgreenland.github.io/pxmake/reference/px_elimination.px.md)
is set, in which case the elimination code is used.

## Usage

``` r
px_add_totals(x, value, na.rm = TRUE, validate)

# S3 method for class 'px'
px_add_totals(x, value, na.rm = TRUE, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  A character vector of variables to add total levels to.

- na.rm:

  Optional. Logical. If TRUE, NAs are removed before summing.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object

## See also

[px_elimination](https://statisticsgreenland.github.io/pxmake/reference/px_elimination.px.md)

## Examples

``` r
# Create small px object example
x0 <- px(subset(population_gl, age == "65+"))
px_data(x0)
#> # A tibble: 6 × 4
#>   gender age   year      n
#>   <chr>  <chr> <chr> <dbl>
#> 1 male   65+   2004   1481
#> 2 male   65+   2014   2238
#> 3 male   65+   2024   3116
#> 4 female 65+   2004   1630
#> 5 female 65+   2014   2004
#> 6 female 65+   2024   2616

# Add total level to one variable
x1 <- px_add_totals(x0, "gender")
px_data(x1)
#> # A tibble: 9 × 4
#>   gender age   year      n
#>   <chr>  <chr> <chr> <dbl>
#> 1 Total  65+   2004   3111
#> 2 Total  65+   2014   4242
#> 3 Total  65+   2024   5732
#> 4 male   65+   2004   1481
#> 5 male   65+   2014   2238
#> 6 male   65+   2024   3116
#> 7 female 65+   2004   1630
#> 8 female 65+   2014   2004
#> 9 female 65+   2024   2616

# Add total level to multiple variables
x2 <- px_add_totals(x0, c("gender", "age"))
px_data(x2)
#> # A tibble: 18 × 4
#>    gender age   year      n
#>    <chr>  <chr> <chr> <dbl>
#>  1 Total  Total 2004   3111
#>  2 Total  Total 2014   4242
#>  3 Total  Total 2024   5732
#>  4 female Total 2004   1630
#>  5 female Total 2014   2004
#>  6 female Total 2024   2616
#>  7 male   Total 2004   1481
#>  8 male   Total 2014   2238
#>  9 male   Total 2024   3116
#> 10 Total  65+   2004   3111
#> 11 Total  65+   2014   4242
#> 12 Total  65+   2024   5732
#> 13 male   65+   2004   1481
#> 14 male   65+   2014   2238
#> 15 male   65+   2024   3116
#> 16 female 65+   2004   1630
#> 17 female 65+   2014   2004
#> 18 female 65+   2024   2616

# The name of the total level can be changed with px_elimination()
x3 <-
  x0 |>
  px_elimination("T") |>
  px_add_totals("gender")

px_data(x3)
#> # A tibble: 9 × 4
#>   gender age   year      n
#>   <chr>  <chr> <chr> <dbl>
#> 1 T      65+   2004   3111
#> 2 T      65+   2014   4242
#> 3 T      65+   2024   5732
#> 4 male   65+   2004   1481
#> 5 male   65+   2014   2238
#> 6 male   65+   2024   3116
#> 7 female 65+   2004   1630
#> 8 female 65+   2014   2004
#> 9 female 65+   2024   2616
```
