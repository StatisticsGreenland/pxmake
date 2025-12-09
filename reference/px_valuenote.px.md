# VALUENOTE

Inspect or change VALUENOTE.

## Usage

``` r
px_valuenote(x, value, validate)

# S3 method for class 'px'
px_valuenote(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame with the columns 'valuenote' and one or more of
  the columns: 'variable-code', 'code', and 'language'. If 'value' is
  missing, the current VALUENOTE is returned. If NULL, VALUENOTE is
  removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object or data frame.

## See also

[Statistics Sweden's
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A55%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C133%2C0%5D)

## Examples

``` r
# Set VALUENOTE for a value
library(tibble)
x1 <-
  population_gl |>
  px() |>
  px_valuenote(
    tribble(~`variable-code`, ~code,  ~valuenote,
            'year', '2004', 'Counts are approximated'))

# Print VALUENOTE
px_valuenote(x1)
#> # A tibble: 1 × 3
#>   `variable-code` code  valuenote              
#>   <chr>           <chr> <chr>                  
#> 1 year            2004  Counts are approximated

# Set VALUENOTE for a value in specific language
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_valuenote(
    tribble(~`variable-code`, ~code,  ~language, ~valuenote,
            'age', '0-6', 'en', 'Some of the figures are from 2003',
            'age', '0-6', 'kl', 'Kisitsisit ilaat 2003-imeersuupput'))
px_valuenote(x2)
#> # A tibble: 4 × 4
#>   `variable-code` code  language valuenote                         
#>   <chr>           <chr> <chr>    <chr>                             
#> 1 age             0-6   en       Some of the figures are from 2003 
#> 2 age             0-6   kl       Kisitsisit ilaat 2003-imeersuupput
#> 3 year            2004  en       Counts are approximated           
#> 4 year            2004  kl       Counts are approximated           

# Remove VALUENOTE
x3 <- px_valuenote(x2, NULL)
px_valuenote(x3)
#> NULL
```
