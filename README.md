
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pxmake

<!-- badges: start -->

![R-CMD-check](https://github.com/StatisticsGreenland/pxmake/actions/workflows/R-CMD-check.yml/badge.svg)
<!-- badges: end -->

pxmake is an R package for creating and modifying px files.

With pxmake you can:

- Import a PC-AXIS px file, modify it, and save it as a new px file.
- Save a px file as an Excel workbook.
- Do complex modifications to a px file, like adding total levels to a
  variable.

## Installation

Install the latest version by running:

``` r
# install.packages('devtools')
devtools::install_github('StatisticsGreenland/pxmake')
```

pxmake is not available on CRAN.

## How to use

Use `px()` and `pxsave()` to read/write a px file to a px object.

``` r
library(pxmake)

# Read px file into a px object
x <- px(input = "example.px")

class(x)
#> [1] "px"

# Save px object as a new px file
pxsave(x, path = "example2.px")
```

To modify a px object, use pxmake’s *modifying functions*.

In general, modifying functions are named after the keyword they modify.
It’s possible to chain multiple modifying functions together in
tidyverse style by using the pipe operator `%>%`.

``` r
library(magrittr) # import pipe

# Create px object from data frame
x <- px(data.frame(year = as.character(rep(2021:2023, each = 3)), 
                   group = c('a', 'b', 'c'), 
                   value = runif(9)
                   )
        ) 

head(x$data, 4)
#>   year group     value
#> 1 2021     a 0.2655087
#> 2 2021     b 0.3721239
#> 3 2021     c 0.5728534
#> 4 2022     a 0.9082078

x %>% 
  timeval("year") %>%  # Set year as TIMEVAL
  heading("year") %>%  # Set year as HEADING
  stub("group") %>%    # Set group as STUB
  decimals("2") %>%    # Set DECIMALS to 2
  pxsave("example.px") # Save as px file
```

### Modifying functions

Currently the following keywords have a modifying function in pxmake:

    #>        Keyword Function name
    #>        CHARSET       charset
    #>  CREATION-DATE creation_date
    #>       DECIMALS      decimals
    #>         MATRIX        matrix
    #>           STUB          stub
    #>        HEADING       heading
    #>        TIMEVAL       timeval

In addition to the above, the following modifying functions are
available:

    #>  Function name
    #>     add_totals
    #>        figures

See the help page for each modifying function for more information.

<details>
<summary>
Keywords without modifying functions
</summary>

These keywords currently doesn’t have a modifying function, but most of
them can be implemented.

    #>              Keyword       Function name
    #>         AXIS-VERSION        axis_version
    #>             CODEPAGE            codepage
    #>             LANGUAGE            language
    #>            LANGUAGES           languages
    #>          NEXT-UPDATE         next_update
    #>            PX-SERVER           px_server
    #>       DIRECTORY-PATH      directory_path
    #>     UPDATE-FREQUENCY    update_frequency
    #>              TABLEID             tableid
    #>             SYNONYMS            synonyms
    #>        DEFAULT-GRAPH       default_graph
    #>         SHOWDECIMALS        showdecimals
    #>             ROUNDING            rounding
    #>        AGGREGALLOWED       aggregallowed
    #>              AUTOPEN             autopen
    #>         SUBJECT-CODE        subject_code
    #>         SUBJECT-AREA        subject_area
    #>         CONFIDENTIAL        confidential
    #>            COPYRIGHT           copyright
    #>          DESCRIPTION         description
    #>                TITLE               title
    #>   DESCRIPTIONDEFAULT  descriptiondefault
    #>             CONTENTS            contents
    #>                UNITS               units
    #>         CONTVARIABLE        contvariable
    #>               VALUES              values
    #>                CODES               codes
    #>         DOUBLECOLUMN        doublecolumn
    #>             PRESTEXT            prestext
    #>               DOMAIN              domain
    #>        VARIABLE-TYPE       variable_type
    #>         VARIABLECODE        variablecode
    #>          HIERARCHIES         hierarchies
    #>      HIERARCHYLEVELS     hierarchylevels
    #>  HIERARCHYLEVELSOPEN hierarchylevelsopen
    #>       HIERARCHYNAMES      hierarchynames
    #>                  MAP                 map
    #>          PARTITIONED         partitioned
    #>          ELIMINATION         elimination
    #>            PRECISION           precision
    #>         LAST-UPDATED        last_updated
    #>              STOCKFA             stockfa
    #>             CFPRICES            cfprices
    #>               DAYADJ              dayadj
    #>              SEASADJ             seasadj
    #>              CONTACT             contact
    #>            REFPERIOD           refperiod
    #>           BASEPERIOD          baseperiod
    #>             DATABASE            database
    #>               SOURCE              source
    #>               SURVEY              survey
    #>                 LINK                link
    #>             INFOFILE            infofile
    #>      FIRST-PUBLISHED     first_published
    #>              META-ID             meta_id
    #>  OFFICIAL-STATISTICS official_statistics
    #>                 INFO                info
    #>                NOTEX               notex
    #>                 NOTE                note
    #>           VALUENOTEX          valuenotex
    #>            VALUENOTE           valuenote
    #>            CELLNOTEX           cellnotex
    #>             CELLNOTE            cellnote
    #>          DATASYMBOL1         datasymbol1
    #>          DATASYMBOL2         datasymbol2
    #>          DATASYMBOL3         datasymbol3
    #>          DATASYMBOL4         datasymbol4
    #>          DATASYMBOL5         datasymbol5
    #>          DATASYMBOL6         datasymbol6
    #>        DATASYMBOLSUM       datasymbolsum
    #>        DATASYMBOLNIL       datasymbolnil
    #>         DATANOTECELL        datanotecell
    #>          DATANOTESUM         datanotesum
    #>             DATANOTE            datanote
    #>                 KEYS                keys
    #>         ATTRIBUTE-ID        attribute_id
    #>       ATTRIBUTE-TEXT      attribute_text
    #>           ATTRIBUTES          attributes
    #>                 DATA                data

</details>

## Need help?

If you have any questions or need help, feel free to [open an
issue](https://github.com/StatisticsGreenland/pxmake/issues/new) on
GitHub, or contact [Lars and
Johan](https://github.com/StatisticsGreenland/pxmake/graphs/contributors)
via email.

## For developers

### PX specification

See the [PX-file format specification on Statistics Swedens
homepage](https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf).

### PxJob

Some tests cases uses
[PxJob](https://www.stat.fi/tup/tilastotietokannat/px-tuoteperhe_en.html).
Install [pxjob64Win](https://github.com/StatisticsGreenland/pxjob64Win)
to be able ro run these tests. This only works on Windows.

### How to create a new release

1.  Checkout ‘main’ branch.
2.  Run `usethis::use_version('major'/'minor'/'patch')`. Answer ‘No’ to
    the prompt ‘Is it ok to commit them?’.
3.  Update NEWS.md with all changes since last release.
4.  Commit changes with message ‘Increment version number to X.Y.Z’.
5.  Run `git tag vX.Y.Z`.
6.  Run `git push`.
7.  Run `git push --tags`.
8.  Run `devtools::use_github_release()`.
9.  Review relase description. If sattified, click the ‘edit’ pen in the
    upper right corner.
10. Click ‘Publish release’.
11. Run `usethis::use_version('dev')`. Answer ‘Yes’ to the prompt ‘Is it
    ok to commit them?’.
12. Run `git push`.
