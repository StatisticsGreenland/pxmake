# VALUES

Inspect or change VALUES.

## Usage

``` r
px_values(x, value, validate)

# S3 method for class 'px'
px_values(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame with the columns 'values' and one or more of
  the columns: 'variable-code', 'code', and 'language'. If 'value' is
  missing, the current VALUES is returned. If NULL, VALUES is removed.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A36%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C212%2C0%5D)

## Examples

``` r
# Set VALUES for a value
library(tibble)
x1 <-
  population_gl |>
  px() |>
  px_values(
    tribble(~`variable-code`, ~code,  ~values,
            'year', '2004', 'Year 2024'))

# Print VALUES
px_values(x1)
#> # A tibble: 10 × 3
#>    `variable-code` code   value    
#>    <chr>           <chr>  <chr>    
#>  1 gender          female female   
#>  2 gender          male   male     
#>  3 age             0-6    0-6      
#>  4 age             17-24  17-24    
#>  5 age             25-64  25-64    
#>  6 age             65+    65+      
#>  7 age             7-16   7-16     
#>  8 year            2004   Year 2024
#>  9 year            2014   2014     
#> 10 year            2024   2024     

# Set VALUES for a value in specific language
x2 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_values(
    tribble(~`variable-code`, ~code,  ~language, ~values,
            'age', '0-6', 'en', 'toddler',
            'age', '0-6', 'kl', 'meeraaqqap'))
px_values(x2)
#> # A tibble: 20 × 4
#>    `variable-code` code   language value     
#>    <chr>           <chr>  <chr>    <chr>     
#>  1 gender          female en       female    
#>  2 gender          female kl       female    
#>  3 gender          male   en       male      
#>  4 gender          male   kl       male      
#>  5 age             0-6    en       toddler   
#>  6 age             0-6    kl       meeraaqqap
#>  7 age             17-24  en       17-24     
#>  8 age             17-24  kl       17-24     
#>  9 age             25-64  en       25-64     
#> 10 age             25-64  kl       25-64     
#> 11 age             65+    en       65+       
#> 12 age             65+    kl       65+       
#> 13 age             7-16   en       7-16      
#> 14 age             7-16   kl       7-16      
#> 15 year            2004   en       Year 2024 
#> 16 year            2004   kl       Year 2024 
#> 17 year            2014   en       2014      
#> 18 year            2014   kl       2014      
#> 19 year            2024   en       2024      
#> 20 year            2024   kl       2024      

# Remove VALUES
x3 <- px_values(x2, NULL)
px_values(x3)
#> NULL
```
