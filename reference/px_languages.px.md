# LANGUAGES

Inspect or change LANGUAGES.

## Usage

``` r
px_languages(x, value, validate)

# S3 method for class 'px'
px_languages(x, value, validate = TRUE)
```

## Arguments

- x:

  A px object

- value:

  Optional. A character vector. If missing, the current LANGUAGES are
  returned. If NULL, LANGUAGES are removed.

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

If LANGUAGE is defined it should be one of the values in LANGUAGES.

If LANGUAGE is set, it is considered the main language. If LANGUAGE is
not set, the first language in LANGUAGES is considered the main
language.

## See also

[`px_language`](https://statisticsgreenland.github.io/pxmake/reference/px_language.px.md)

## Examples

``` r
# Set LANGUAGES to 'en' and 'kl', with 'en' as main language
x1 <-
  population_gl |>
  px() |>
  px_languages(c('en', 'kl'))

# Print LANGUAGES
px_languages(x1)
#> [1] "en" "kl"

# Remove LANGUAGES
x2 <- px_languages(x1, NULL)
px_languages(x2)
#> NULL
```
