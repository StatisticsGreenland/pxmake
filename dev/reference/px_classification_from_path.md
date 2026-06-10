# px classification from path

Create px classification from .vs and .agg files

## Usage

``` r
px_classification_from_path(vs_path, agg_paths)
```

## Arguments

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

A classification object
