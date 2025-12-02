# First PX-file

To create your first PX-file with pxmake, you can either start with an
existing PX-file, or use a data set.

## How to create a PX-file from a data set

All workflows in pxmake, starts the same way: by using the
[`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
to create a *px object*. Here we are using the built in dataset
`population_gl`.

``` r
library(pxmake)

population_gl |>
  head(10) |>
  print()
#> # A tibble: 10 × 4
#>    gender age   year      n
#>    <fct>  <chr> <chr> <dbl>
#>  1 male   0-6   2004   3237
#>  2 male   0-6   2014   2950
#>  3 male   0-6   2024   2769
#>  4 male   7-16  2004   5085
#>  5 male   7-16  2014   4040
#>  6 male   7-16  2024   3865
#>  7 male   17-24 2004   3191
#>  8 male   17-24 2014   3614
#>  9 male   17-24 2024   2940
#> 10 male   25-64 2004  17328
```

To create the px object, simply pass the data set to the
[`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
function.

``` r
x <- px(population_gl)
```

`x` is now a px object. The word *object* just means that the PX-file is
stored in a specific format that all functions in the pxmake package
uses.

Use
[`px_save()`](https://statisticsgreenland.github.io/pxmake/reference/px_save.md)
to save the px object as a PX-file.

``` r
px_save(x, "population_gl.px")
```

Since we provided no metadata,
[`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
added a minimal set of metadata, which is necessary for creating a valid
PX-file.

The resulting PX-file can be seen below.

    CHARSET="ANSI";
    DECIMALS=0;
    MATRIX="matrix";
    SUBJECT-CODE="subject-code";
    SUBJECT-AREA="subject-area";
    TITLE="";
    CONTENTS="contents";
    STUB="gender","age";
    HEADING="year";
    VALUES("gender")="female","male";
    VALUES("age")="0-6","17-24","25-64","65+","7-16";
    VALUES("year")="2004","2014","2024";
    UNITS="units";
    CODES("gender")="female","male";
    CODES("age")="0-6","17-24","25-64","65+","7-16";
    CODES("year")="2004","2014","2024";
    VARIABLECODE("gender")="gender";
    VARIABLECODE("age")="age";
    VARIABLECODE("year")="year";
    VARIABLECODE("n")="n";
    DATA=
    3109 2644 2668
    3003 3528 2862
    13744 14397 15098
    1630 2004 2616
    5018 3979 3534
    3237 2950 2769
    3191 3614 2940
    17328 16888 17231
    1481 2238 3116
    5085 4040 3865
    ;

## How to create a PX-file from a PX-file

If you already have a PX-file, that you want to manipulate using pxmake,
simple pass the path of the file, to the
[`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
function.

``` r
x2 <- px("population_gl.px")
```

Just as with the dataset, `x2` is now a px object and can be saved or
modified using the other functions in pxmake.

## Modifying a px object

The real fun starts when you start modifying the px object, using one of
pxmake many built in functions.

In general, each px keyword has a corresponding function in pxmake. For
example, to change the title of the PX-file, you can use the
[`px_title()`](https://statisticsgreenland.github.io/pxmake/reference/px_title.px.md)
function.

``` r
x3 <- px_title(x, "Population in Greenland")
```

The
[`px_title()`](https://statisticsgreenland.github.io/pxmake/reference/px_title.px.md)
function returns a new px object, with the title changed. The original
px object is not modified.

``` r
x3 |>
  px_codepage("UTF-8") |> # Change file encoding  
  px_matrix("pop") |>
  px_contact("Johan Ejstrud") |>
  px_subject_code("GL") |>
  px_subject_area("Greenland") |>
  px_timeval("year") |>
  px_contents("Population in Greenland") |>
  px_units("People") |>
  px_note("See information about data: ?population_gl") |>
  px_last_updated(format(Sys.time(), "%Y%m%d %H:%M")) |>
  px_stub(c("age", "gender")) |> # Change order of STUB variables
  px_save("population_gl_modified.px")
```

The resulting PX-file can be seen below.

    CHARSET="ANSI";
    CODEPAGE="UTF-8";
    DECIMALS=0;
    MATRIX="pop";
    SUBJECT-CODE="GL";
    SUBJECT-AREA="Greenland";
    TITLE="Population in Greenland";
    CONTENTS="Population in Greenland";
    STUB="age","gender";
    HEADING="year";
    VALUES("age")="0-6","17-24","25-64","65+","7-16";
    VALUES("gender")="female","male";
    VALUES("year")="2004","2014","2024";
    UNITS="People";
    TIMEVAL("year")=TLIST(A1),"2004","2014","2024";
    CODES("age")="0-6","17-24","25-64","65+","7-16";
    CODES("gender")="female","male";
    CODES("year")="2004","2014","2024";
    VARIABLECODE("age")="age";
    VARIABLECODE("gender")="gender";
    VARIABLECODE("year")="year";
    VARIABLECODE("n")="n";
    LAST-UPDATED="20251202 16:20";
    CONTACT="Johan Ejstrud";
    NOTE="See information about data: ?population_gl";
    DATA=
    3109 2644 2668
    3237 2950 2769
    3003 3528 2862
    3191 3614 2940
    13744 14397 15098
    17328 16888 17231
    1630 2004 2616
    1481 2238 3116
    5018 3979 3534
    5085 4040 3865
    ;

## Advanced use

See the other vignettes for more advanced use cases: -
[`vignette("languages")`](https://statisticsgreenland.github.io/pxmake/articles/languages.md) -
[`vignette("micro-files")`](https://statisticsgreenland.github.io/pxmake/articles/micro-files.md)
