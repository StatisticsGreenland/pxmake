# Create new classification object

Constructor for internal functions

## Usage

``` r
new_classification(name, prestext, domain, df)
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
