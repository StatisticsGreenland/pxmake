# Create a classification object

Create a classification object from a data frame or .vs and .agg files.

## Usage

``` r
px_classification(name, prestext, domain, df, vs_path, agg_paths)
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

A classification object

## Details

A classification is a combination of a value set and zero, one, or more
aggregations. The classification can be saved as .vs and .agg files (see
[`px_save_classification()`](https://statisticsgreenland.github.io/pxmake/reference/px_save_classification.md)).

If a classification is created from a data frame, the arguments `name`
and `prestext` and `domain` are required. If a classification is created
from .vs and .agg files, all other arguments should be empty.

For aggregations, it's normally possible to have codes and values in the
.agg file under the sections '\[Aggreg\]' and '\[Aggtext\]'
respectively. However, in pxmake's implementation of classifications,
it's not possible to distinguish between these. When a new
classification is created, the values in the section '\[Aggtext\]' are
used as aggregation values.

Only value sets of type 'V' are supported. Type values sets with type
'H' and 'N' are not supported.

## Examples

``` r
# Create classification from data frame
library(tibble)

c1 <- px_classification(name = "Age5",
                        prestext = "Ages 0-9 - 60+",
                        domain = "age",
                        df = tribble(
                           ~valuecode,    ~valuetext,   ~`25 years classes`,
                                "0-4",     "0-4 years",              "0-24",
                                "5-9",     "5-9 years",              "0-24",
                              "10-14",   "10-14 years",              "0-24",
                              "15-19",   "15-19 years",              "0-24",
                              "20-24",   "20-24 years",              "0-24",
                              "25-29",   "25-29 years",             "25-49",
                              "30-34",   "30-34 years",             "25-49",
                              "35-39",   "35-39 years",             "25-49",
                              "40-44",   "40-44 years",             "25-49",
                              "45-49",   "45-49 years",             "25-49",
                              "50-54",   "50-54 years",             "50-74",
                              "55-59",   "55-59 years",             "50-74",
                              "60-64",   "60-64 years",             "50-74",
                              "65-69",   "65-69 years",             "50-74",
                              "70-74",   "70-74 years",             "50-74",
                                "75+",     "75+ years",               "75+"
                                )
                        )

# Create classifications from files

vs_file <- system.file("extdata", "Age5.vs", package = "pxmake")

agg_files <- c(
  system.file("extdata", "10-years_classes.agg", package = "pxmake"),
  system.file("extdata", "25-years_classes.agg", package = "pxmake")
)

if (vs_file != "" & all(agg_files != "")) {
  # Create classification from .vs file and use aggregations mentioned in .vs
  c2 <- px_classification(vs_path = vs_file)

  # Create classification from .vs file and manually specify aggregation files
  c3 <- px_classification(vs_path = vs_file,
                          agg_paths = agg_files
                          )

  identical(c2, c3)
}
#> [1] TRUE
```
