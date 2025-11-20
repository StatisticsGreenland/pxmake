# Save classification as .vs and .agg files

Save a classification object as .vs and .agg files. The .vs file
contains the value set and the .agg files contain the aggregations.

## Usage

``` r
px_save_classification(c, path)
```

## Arguments

- c:

  A classification object

- path:

  Directory to save the files in

## Value

Nothing.

## Examples

``` r
# Save classification as .vs as .agg files
c <- px_classification(name = "Age5",
                       prestext = "Ages 0-9 - 60+",
                       domain = "age",
                       df = age_classification
                       )

px_save_classification(c, path = tempdir())
```
