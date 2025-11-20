# AUTOPEN

Inspect or change AUTOPEN.

## Usage

``` r
px_autopen(x, value, validate)

# S3 method for class 'px'
px_autopen(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string. If missing, the current AUTOPEN is
  returned. If NULL, AUTOPEN is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A39%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C261%2C0%5D)

## Examples

``` r
# Set AUTOPEN
x1 <-
   px(population_gl) |>
   px_autopen('YES')

# Print AUTOPEN
px_autopen(x1)
#> [1] "YES"

# Remove AUTOPEN
x2 <- px_autopen(x1, NULL)
px_autopen(x2)
#> NULL
```
