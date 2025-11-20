# px classification from data frame

Create px classification from a data frame.

## Usage

``` r
px_classification_from_df(name, prestext, domain, df)
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

## Value

A classification object
