# CELLNOTE

Inspect or change CELLNOTE.

## Usage

``` r
px_cellnote(x, value, na_to_star, validate)

# S3 method for class 'px'
px_cellnote(x, value, na_to_star = TRUE, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame with columns 'cellnote' and one or more columns
  with the names of the STUB and HEADING variables. The 'cellnote'
  column is the cellnote text, and the STUB/HEADING columns control
  which cells the note applies to. Use star (\*) if a note applies to
  all cells in a variable. Use column 'language' to set CELLNOTE for
  specific languages. If 'value' is missing, the current CELLNOTE is
  returned. If value is NULL, CELLNOTE is removed.

- na_to_star:

  Optional. Convert all NAs to '\*'.

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
documentation](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf#%5B%7B%22num%22%3A40%2C%22gen%22%3A0%7D%2C%7B%22name%22%3A%22XYZ%22%7D%2C184%2C460%2C0%5D)

## Examples

``` r
# Set CELLNOTE for a value
library(tibble)
x1 <-
  population_gl |>
  px() |>
  px_cellnote(
    tribble(~gender,  ~age,  ~year, ~cellnote,
             'male', '0-6', '2004', 'Approximation'))

x2 <-
  x1 |>
  px_cellnote(
    tribble(~gender,   ~age,  ~year, ~cellnote,
           'female',    '*', '2014', 'Uncertainty in ages'))

# Print CELLNOTE
px_cellnote(x2)
#> # A tibble: 2 × 4
#>   gender age   year  cellnote           
#>   <chr>  <chr> <chr> <chr>              
#> 1 male   0-6   2004  Approximation      
#> 2 female *     2014  Uncertainty in ages

# Set CELLNOTE in multiple languagese
x3 <-
  x1 |>
  px_languages(c('en', 'kl')) |>
  px_cellnote(
    tribble(~age, ~year, ~language, ~cellnote,
             '*', '2003',  'en', 'Some of the figures are from 2003',
             '*', '2003', 'kl', 'Kisitsisit ilaat 2003-imeersuupput'))
px_cellnote(x3)
#> # A tibble: 4 × 5
#>   gender age   year  language cellnote                          
#>   <chr>  <chr> <chr> <chr>    <chr>                             
#> 1 male   0-6   2004  en       Approximation                     
#> 2 male   0-6   2004  kl       Approximation                     
#> 3 *      *     2003  en       Some of the figures are from 2003 
#> 4 *      *     2003  kl       Kisitsisit ilaat 2003-imeersuupput

# Remove CELLNOTE
x4 <- px_cellnote(x3, NULL)
px_cellnote(x4)
#> NULL
```
