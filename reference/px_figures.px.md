# Change figures variable

Inspect or change which variable is used as figures. The previous
figures variable is changed to STUB. There can only be one figures
variable.

## Usage

``` r
px_figures(x, value, validate)

# S3 method for class 'px'
px_figures(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. Name of variable to use as FIGRUES. If missing, the current
  PX_FIGURES variable is returned.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or a character string

## See also

[`px_stub`](https://statisticsgreenland.github.io/pxmake/reference/px_stub.px.md)
[`px_heading`](https://statisticsgreenland.github.io/pxmake/reference/px_heading.px.md)

## Examples

``` r
x1 <- px(population_gl)

# Print FIGURES
px_figures(x1)
#> [1] "n"

# Change 'age' to FIGURES variable, 'n' i changed to STUB
x2 <- px_figures(x1, 'age')
px_figures(x2)
#> [1] "age"
px_stub(x2)
#> [1] "n"      "gender"
```
