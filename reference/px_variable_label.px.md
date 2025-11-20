# Change VARIABLE-LABEL

Inspect or change VARIABLE-LABEL.

The variable label is the name that is shown in the PX-file.

## Usage

``` r
px_variable_label(x, value, validate)

# S3 method for class 'px'
px_variable_label(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string or data frame.

  - Use character to set VARIABLE-LABEL for all languages and
    STUB/HEADING variables.

  - Use data frame with columns 'variable-code', 'language' and
    'variable-label' to set VARIABLE-LABEL for specific variables.

  - If missing, the current VARIABLE-LABEL is returned.

  - If NULL, VARIABLE-LABEL is removed for all variables.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object, a character string, or a data frame.

## Examples

``` r
# Set VARIABLE-LABEL for individual variables
library(tibble)
x1 <-
  px(population_gl) |>
  px_variable_label(tribble(~`variable-code`, ~`variable-label`,
                            'gender',         'Gender',
                            'age',            'Age'))
px_variable_label(x1)
#> # A tibble: 4 × 2
#>   `variable-code` `variable-label`
#>   <chr>           <chr>           
#> 1 gender          Gender          
#> 2 age             Age             
#> 3 year            year            
#> 4 n               n               

# Set VARIABLE-LABEL for individual languages
x2 <-
  x1 %>%
  px_languages(c('en', 'kl')) |>
  px_variable_label(tribble(~`variable-code`, ~language, ~`variable-label`,
                            'gender',         'en',      'Gender',
                            'gender',         'kl',      'Suiaassuseq',
                            'age',            'en',      'Age',
                            'age',            'kl',      'Ukiut'))
px_variable_label(x2)
#> # A tibble: 8 × 3
#>   `variable-code` language `variable-label`
#>   <chr>           <chr>    <chr>           
#> 1 gender          en       Gender          
#> 2 gender          kl       Suiaassuseq     
#> 3 age             en       Age             
#> 4 age             kl       Ukiut           
#> 5 year            en       year            
#> 6 year            kl       year            
#> 7 n               en       n               
#> 8 n               kl       n               

# Remove VARIABLE-LABEL
x3 <- px_variable_label(x2, NULL)
px_variable_label(x3)
#> NULL
```
