# AXIS-VERSION

Inspect or change AXIS-VERSION.

## Usage

``` r
px_axis_version(x, value, validate)

# S3 method for class 'px'
px_axis_version(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string. If missing, the current AXIS-VERSION is
  returned. If NULL, AXIS-VERSION is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A40%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C699%2C0%5D)

## Examples

``` r
# Set AXIS-VERSION
x1 <-
   px(population_gl) |>
   px_axis_version('2010')

# Print AXIS-VERSION
px_axis_version(x1)
#> [1] "2010"

# Remove AXIS-VERSION
x2 <- px_axis_version(x1, NULL)
px_axis_version(x2)
#> NULL
```
