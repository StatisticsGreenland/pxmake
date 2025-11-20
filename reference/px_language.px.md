# LANGUAGE

Inspect or change LANGUAGE.

## Usage

``` r
px_language(x, value, validate)

# S3 method for class 'px'
px_language(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character string. If missing, the current LANGUAGE is
  returned. If NULL, LANGUAGE is removed.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

A px object

## Details

If LANGUAGES is defined, changing LANGUAGE will also add is to
LANGUAGES.

## See also

[`px_languages`](https://statisticsgreenland.github.io/pxmake/reference/px_languages.px.md)

## Examples

``` r
# Set LANGUAGE to 'en'
x1 <-
  population_gl |>
  px() |>
  px_language('en')

# Print LANGUAGE
px_language(x1)
#> [1] "en"

# Remove LANGUAGE
x2 <- px_language(x1, NULL)
px_language(x2)
#> NULL
```
