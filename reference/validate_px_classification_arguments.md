# Check all arguments to px_classification

Check all arguments to px_classification

## Usage

``` r
validate_px_classification_arguments(
  name,
  prestext,
  domain,
  df,
  vs_path,
  agg_paths
)
```

## Arguments

- name:

  Optional. Name of the classification.

- prestext:

  Optional. Presentation text.

- domain:

  Optional. Character vector with domain names. Used to link to PX-file.

- df:

  Optional. A data frame with required column 'valuecode' and optional
  column 'valuetext', if the codes have texts. Each additional column
  represents an aggregation. The column name is the name of the
  aggregation. If the column type is character the aggregation levels
  will be sorted alphabetically; use factors to control the ordering.

- vs_path:

  Optional. Path to a values set (.vs) file.

- agg_paths:

  Optional.

  - If NULL, aggregation paths are automatically taken from the
    \[Aggreg\] field in the .vs file.

  - Use a vector of paths to one or more aggregation files (.agg) to
    manually choose aggregations.

  - Use character(0) if aggregations from the .vs files should not be
    added automatically.

## Value

Nothing
