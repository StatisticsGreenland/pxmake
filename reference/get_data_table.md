# Return data table from px object

Return data table from px object

## Usage

``` r
get_data_table(x, value, labels, sort)
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
