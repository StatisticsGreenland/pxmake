# HEADING

Inspect or change HEADING.

## Usage

``` r
px_heading(x, value, validate)

# S3 method for class 'px'
px_heading(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character vector of variable names to change to STUB. This
  also changes the HEADING order. With names in `variables` becoming 1,
  2, ... If missing, the current HEADING variables are returned.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or a character vector.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A35%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C610%2C0%5D)

[`px_stub`](https://statisticsgreenland.github.io/pxmake/reference/px_stub.px.md)
[`px_figures`](https://statisticsgreenland.github.io/pxmake/reference/px_figures.px.md)

## Examples

``` r
x1 <- px(population_gl)

# Print HEADING
px_heading(x1)
#> [1] "year"

# Add 'gender' to HEADING
x2 <- px_heading(x1, 'gender')
px_heading(x2)
#> [1] "gender" "year"  

# Change order of HEADING
x3 <- px_heading(x2, 'year')
px_heading(x3)
#> [1] "year"   "gender"
```
