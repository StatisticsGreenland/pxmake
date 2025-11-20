# Check px object

Runs a number of checks on px object to see if it is valid.

## Usage

``` r
px_validate(x)
```

## Arguments

- x:

  A supposed px object.

## Value

A valid px object.

## Details

This check is run by default by all `px_*` functions, but can be skipped
by using `validate = FALSE`. This can be useful on large px objects
where the checks are time consuming. Instead of validating on every
modifying function `px_validate()` can be run as the final step to
validate the object.

## Examples

``` r
# Turn off validation for modifying functions, and manually
# run validation as final step in creating px object.
x1 <-
  px(population_gl, validate = FALSE) |>
  px_title("Test", validate = FALSE) |>
  px_validate()
```
