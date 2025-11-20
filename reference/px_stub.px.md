# STUB

Inspect or change STUB.

## Usage

``` r
px_stub(x, value, validate)

# S3 method for class 'px'
px_stub(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character vector of variable names to change to STUB. This
  also changes the STUB order. With names in `variables` becoming 1, 2,
  ... If missing, the current STUB variables are returned.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A35%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C329%2C0%5D)

[`px_heading`](https://statisticsgreenland.github.io/pxmake/reference/px_heading.px.md)
[`px_figures`](https://statisticsgreenland.github.io/pxmake/reference/px_figures.px.md)

## Examples

``` r
x1 <- px(population_gl)
# Print STUB
px_stub(x1)
#> [1] "gender" "age"   
# Add 'year' to STUB
x2 <- px_stub(x1, 'year')
px_stub(x2)
#> [1] "year"   "gender" "age"   

# Change order of STUB
x3 <- px_stub(x2, c('age', 'gender'))
px_stub(x3)
#> [1] "age"    "gender" "year"  
```
