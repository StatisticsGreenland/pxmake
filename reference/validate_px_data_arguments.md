# Check all arguments to px_data

Check all arguments to px_data

## Usage

``` r
validate_px_data_arguments(x, value, labels, sort, validate)
```

## Arguments

- x:

  A px object

- value:

  Optional. A data frame. If missing, the current DATA is returned. If
  NULL, all data rows are removed.

- labels:

  Optional. Logic or character vector. If TRUE, the data table is
  returned with VALUES instead of CODES. By default the VALUES of the
  main language are returned, use a character language code to return
  VALUES for a specific language.

- sort:

  Optional. If TRUE, the data table is returned in the sort order
  defined by
  [`px_order()`](https://statisticsgreenland.github.io/pxmake/reference/px_order.px.md).
  If FALSE, the data table is returned as is.

- validate:

  Optional. If TRUE a number of validation checks are performed on the
  px object, and an error is thrown if the object is not valid. If
  FALSE, the checks are skipped, which can be usefull for large px
  objects where the check can be time consuming. Use
  [`px_validate()`](https://statisticsgreenland.github.io/pxmake/reference/px_validate.md)
  to manually preform the check.

## Value

Nothing
