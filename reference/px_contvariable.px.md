# CONTVARIABLE

Inspect or change CONTVARIABLE.

Setting CONTVARIABLE indexes several variables in table2. Removing
CONTVARIABLE removes the indexing from table2.

## Usage

``` r
px_contvariable(x, value, validate)

# S3 method for class 'px'
px_contvariable(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string. If missing, the current CONTVARIABLE is
  returned. If NULL, CONTVARIABLE is removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or a character string.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A42%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C484%2C0%5D)

## Examples

``` r
# Set CONTVARIABLE
x1 <-
  px(population_gl) |>
  px_contvariable('gender')

# After setting CONTVARIABLE some variables are index by it, e.g. UNITS
px_units(x1)
#> # A tibble: 3 × 3
#>   code   language value
#>   <chr>  <chr>    <chr>
#> 1 NA     NA       units
#> 2 female NA       units
#> 3 male   NA       units

# Remove CONTVARIABLE
x2 <- px_contvariable(x1, NULL)
px_contvariable(x2)
#> NULL

# Removing CONTVARIABLE also removes the index from UNITS
px_units(x2)
#> [1] "units"
```
