# Changelog

## pxmake (development version)

### Bug fixes and minor improvements

- Micro files with multiple headings now only removes a heading if it is
  NA across all other headings.
- Add support for short TIMEVAL syntax.
  ([\#435](https://github.com/StatisticsGreenland/pxmake/issues/435))
- Bugfix: Add MAP to variables2 instead of table2. (\$409)
- Add support for OFFICIAL_STATISTICS keyword.
  ([\#443](https://github.com/StatisticsGreenland/pxmake/issues/443))
- Add support for direct link download URL as input to
  [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md).
  ([\#445](https://github.com/StatisticsGreenland/pxmake/issues/445))
- Fix namespace issue with px_keywords data set.
  ([\#358](https://github.com/StatisticsGreenland/pxmake/issues/358))
- Preserve VALUENOTE(X) when making file multilingual.
  ([\#363](https://github.com/StatisticsGreenland/pxmake/issues/363))

## pxmake 0.18.0

CRAN release: 2025-05-30

### New features

- [`px_data()`](https://statisticsgreenland.github.io/pxmake/reference/px_data.px.md)
  has a new argument, `labels`, which makes the function return the data
  table with VALUES instead of CODES.
  ([\#419](https://github.com/StatisticsGreenland/pxmake/issues/419))

### Breaking changes

- ELIMINATION is changed from language dependent to not language
  dependent, and instead of adding ELIMINATION as values, it is done
  with codes.
  ([\#406](https://github.com/StatisticsGreenland/pxmake/issues/406))

### Bug fixes and minor improvements

- Bugfix when saving as R scripts.
  ([\#410](https://github.com/StatisticsGreenland/pxmake/issues/410))
- Elimination values and order are preserved when changing data table.
  ([\#418](https://github.com/StatisticsGreenland/pxmake/issues/418),
  [\#420](https://github.com/StatisticsGreenland/pxmake/issues/420))

## pxmake 0.17.0

CRAN release: 2025-04-07

### Bug fixes and minor improvements

- Remove import of ‘pillar’ package.
  ([\#387](https://github.com/StatisticsGreenland/pxmake/issues/387))
- Fix various issues with CONTVARIABLE.
  ([\#364](https://github.com/StatisticsGreenland/pxmake/issues/364)
  [\#386](https://github.com/StatisticsGreenland/pxmake/issues/386))
- [`px_data()`](https://statisticsgreenland.github.io/pxmake/reference/px_data.px.md)
  updates metadata to match new data frame.
  ([\#385](https://github.com/StatisticsGreenland/pxmake/issues/385)
  [\#396](https://github.com/StatisticsGreenland/pxmake/issues/396)
  [\#399](https://github.com/StatisticsGreenland/pxmake/issues/399))
- When adding more languages dummy values are added as VALUES instead of
  blanks.
- Throw error when using
  [`px_save()`](https://statisticsgreenland.github.io/pxmake/reference/px_save.md)
  on a px object that contains duplicates in the data table.
  ([\#402](https://github.com/StatisticsGreenland/pxmake/issues/402))
- Don’t save NA values in aggregations files.
  ([\#401](https://github.com/StatisticsGreenland/pxmake/issues/401))

### Breaking changes

- Create aggregations from texts in section \[Aggtext\].
  ([\#389](https://github.com/StatisticsGreenland/pxmake/issues/389))

## pxmake 0.16.0

CRAN release: 2025-03-04

### New features

- When creating px object from data frame the ordering in cells1 is
  determined based on the type in the data frame.

### Breaking changes

- Require R version \>= 4.1.
  ([\#376](https://github.com/StatisticsGreenland/pxmake/issues/376))
- Store all non-figures variables in data table as character instead of
  as factors.
  ([\#375](https://github.com/StatisticsGreenland/pxmake/issues/375))

### Bug fixes and minor improvements

- Make tests an examples ‘fail gracefully’ if URL can’t be reached, to
  comply with CRAN policies.
  ([\#379](https://github.com/StatisticsGreenland/pxmake/issues/379))

## pxmake 0.15.1

CRAN release: 2025-02-17

### Bug fixes and minor improvements

- Remove invalid URL from
  [`px_variable_label()`](https://statisticsgreenland.github.io/pxmake/reference/px_variable_label.px.md)
  documentation.

## pxmake 0.15.0

### New features

- Variable order is preserved as it appears in the data set.
  ([\#345](https://github.com/StatisticsGreenland/pxmake/issues/345))
- [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
  supports URLs as input.
  ([\#353](https://github.com/StatisticsGreenland/pxmake/issues/353))
- [`px_save()`](https://statisticsgreenland.github.io/pxmake/reference/px_save.md)
  can save px object as an R script that, when run, regenerates the px
  object. The implementation is not fully reliable; in some cases,
  running the generated R script produces a slightly different px
  object.
  ([\#350](https://github.com/StatisticsGreenland/pxmake/issues/350))

### Bug fixes and minor improvements

- Bugfix:
  [`px_micro()`](https://statisticsgreenland.github.io/pxmake/reference/px_micro.md)
  sometimes ordered variable references incorrectly in CELLNOTES.
  ([\#348](https://github.com/StatisticsGreenland/pxmake/issues/348))
- Removed `dontrun` example from
  [`px_classification()`](https://statisticsgreenland.github.io/pxmake/reference/px_classification.md)
  by updating package files.
  ([\#346](https://github.com/StatisticsGreenland/pxmake/issues/346))
- Use *main language* VALUES as CODES in PX-files without CODES.
  ([\#361](https://github.com/StatisticsGreenland/pxmake/issues/361))

## pxmake 0.14.2

CRAN release: 2025-01-22

- Add CRAN note explaning why /dontrun{} is used.

## pxmake 0.14.1

### Bug fixes and minor improvements

- Fix one CRAN submission related issue, and add a note about another.

## pxmake 0.14.0

### New features

- Prepare for CRAN submission.
  ([\#330](https://github.com/StatisticsGreenland/pxmake/issues/330))
- Add `validate =` option to all modifying functions to enable turning
  off validation.
  ([\#332](https://github.com/StatisticsGreenland/pxmake/issues/332))
- Add functions
  [`px_classification()`](https://statisticsgreenland.github.io/pxmake/reference/px_classification.md)
  and `px_classification_save()` to create and save classifications
  (value sets and aggregations).
  ([\#327](https://github.com/StatisticsGreenland/pxmake/issues/327))

### Bug fixes and minor improvements

- [`px_elimination()`](https://statisticsgreenland.github.io/pxmake/reference/px_elimination.px.md),
  [`px_map()`](https://statisticsgreenland.github.io/pxmake/reference/px_map.px.md),
  and
  [`px_domain()`](https://statisticsgreenland.github.io/pxmake/reference/px_domain.px.md)
  now only sets value for STUB/HEADING variables if value is character.
  ([\#325](https://github.com/StatisticsGreenland/pxmake/issues/325))
- Optimize
  [`px_micro()`](https://statisticsgreenland.github.io/pxmake/reference/px_micro.md)
  by only setting stub once, instead of for all files.
- Add `na_to_star` option to
  [`px_cellnote()`](https://statisticsgreenland.github.io/pxmake/reference/px_cellnote.px.md)
  and
  [`px_cellnotex()`](https://statisticsgreenland.github.io/pxmake/reference/px_cellnotex.px.md).
  ([\#335](https://github.com/StatisticsGreenland/pxmake/issues/335))
- Support PX-files with cell values that contain closing parentheses.
  ([\#337](https://github.com/StatisticsGreenland/pxmake/issues/337))
- Avoid all R CMD Check notes, warning and errors.
- Add CRAN installation instruction.
- Use name ‘PX-files’ consistently.
- Update package title and description.
- Add [@examples](https://github.com/examples) and
  [@return](https://github.com/return) for all exported functions.
- Reduce size of large files.

## pxmake 0.12.1

### New features

- [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
  and
  [`px_save()`](https://statisticsgreenland.github.io/pxmake/reference/px_save.md)
  can write and read to and from `.parquet` files.
- Change default value for all mandatory keywords to be descriptive
  rather than empty string ““. This was done because PxWin does not
  allow empty strings in mandatory keywords.
  ([\#320](https://github.com/StatisticsGreenland/pxmake/issues/320))
- Improve implementation and fix bugs in
  [`px_cellnote()`](https://statisticsgreenland.github.io/pxmake/reference/px_cellnote.px.md)
  and
  [`px_cellnotex()`](https://statisticsgreenland.github.io/pxmake/reference/px_cellnotex.px.md).
  ([\#289](https://github.com/StatisticsGreenland/pxmake/issues/289))

### Bug fixes and minor improvements

- Keyword UNITS is now placed after CONTVARIABLE and VALUES in the
  PX-file.
  ([\#308](https://github.com/StatisticsGreenland/pxmake/issues/308))
- Add default values for mandatory keywords in all languages.
  ([\#219](https://github.com/StatisticsGreenland/pxmake/issues/219))
- Bugfix
  [`px_language()`](https://statisticsgreenland.github.io/pxmake/reference/px_language.px.md)
  stops removing previously defined languages.
  ([\#297](https://github.com/StatisticsGreenland/pxmake/issues/297))
- Change sorting order of PX-file, so variables within keywords are in
  stub/heading order instead of alphabetic.
  ([\#264](https://github.com/StatisticsGreenland/pxmake/issues/264))
- Throw error if any value contains quotation marks (“).
  ([\#238](https://github.com/StatisticsGreenland/pxmake/issues/238))
- Allow minimal px object to be created even if no input is provided.
  ([\#234](https://github.com/StatisticsGreenland/pxmake/issues/234))
- Bugfix
  [`px_micro()`](https://statisticsgreenland.github.io/pxmake/reference/px_micro.md)
  used undefined function.
  ([\#290](https://github.com/StatisticsGreenland/pxmake/issues/290))
- Changing figures removes previous figures variable form cells1 and
  cells2.
  ([\#300](https://github.com/StatisticsGreenland/pxmake/issues/300))
- Add validation check that CONTVARIABLE and TIMEVAL has to be STUB or
  HEADING variables.
  ([\#305](https://github.com/StatisticsGreenland/pxmake/issues/305))
- Add pxmake logo.
- Keyword MAP is now stored in variables2 instead of table2.
  ([\#295](https://github.com/StatisticsGreenland/pxmake/issues/295))

## pxmake 0.12.0

### Breaking changes

- Change argument ‘variable’ and ‘variables’ to ‘value’ in
  [`px_stub()`](https://statisticsgreenland.github.io/pxmake/reference/px_stub.px.md),
  [`px_heading()`](https://statisticsgreenland.github.io/pxmake/reference/px_heading.px.md),
  [`px_figures()`](https://statisticsgreenland.github.io/pxmake/reference/px_figures.px.md),
  [`px_timeval()`](https://statisticsgreenland.github.io/pxmake/reference/px_timeval.px.md),
  [`px_add_totals()`](https://statisticsgreenland.github.io/pxmake/reference/px_add_totals.px.md),
  to align with all other modifying functions.
  ([\#296](https://github.com/StatisticsGreenland/pxmake/issues/296))

### New features

- Add modifying functions:
  [`px_values()`](https://statisticsgreenland.github.io/pxmake/reference/px_values.px.md),
  [`px_variable_label()`](https://statisticsgreenland.github.io/pxmake/reference/px_variable_label.px.md),
  [`px_data()`](https://statisticsgreenland.github.io/pxmake/reference/px_data.px.md),
  [`px_source()`](https://statisticsgreenland.github.io/pxmake/reference/px_source.px.md).
  ([\#291](https://github.com/StatisticsGreenland/pxmake/issues/291))
- Add pkgdown version of documentation on:
  <https://statisticsgreenland.github.io/pxmake/>
- Add example data sets `population_gl` and `greenlanders`.
  ([\#282](https://github.com/StatisticsGreenland/pxmake/issues/282))
- [`px_micro()`](https://statisticsgreenland.github.io/pxmake/reference/px_micro.md)
  support multilingual `keyword_values` arguments.
  ([\#306](https://github.com/StatisticsGreenland/pxmake/issues/306))
- Add example section to most help pages.

### Bug fixes and minor improvements

- Update
  [`px_micro()`](https://statisticsgreenland.github.io/pxmake/reference/px_micro.md)
  documentation.
- Bugfix:
  [`px_add_totals()`](https://statisticsgreenland.github.io/pxmake/reference/px_add_totals.px.md)
  now uses default value ‘Total’ if `x$variables2$elimination` is `NA`.
  ([\#284](https://github.com/StatisticsGreenland/pxmake/issues/284))
- Add `na.rm` argument to `px_add_totalts()`.
- [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
  can now take an Excel workbook without a ‘Data’ sheet, without
  requiring that argument ‘data’ is supplied.
- Remove unused functions.
- Add three vigenttes.
  ([\#277](https://github.com/StatisticsGreenland/pxmake/issues/277),
  [\#278](https://github.com/StatisticsGreenland/pxmake/issues/278),
  [\#280](https://github.com/StatisticsGreenland/pxmake/issues/280))
- Increase test coverage.
- Move px keywords to internal data set.
- Add link to Statstics Sweden’s documentation in help page for all px
  keywords.
- Add Statistics Greenland vignette.

## pxmake 0.11.0

This release contains *a lot* of breaking changes. This was done because
a few functions had name clashes with popular packages. The prefix
‘px\_’ was added to almost all functions to avoid this.

Because of this massive breaking change, a few other breaking changes
were made as well, to create more intuitive naming and make the package
more consistent.

### How to update code from before version 0.11.0

- Rename functions
  - Change all modifying functions by adding the prefix `px_`. E.g.
    change `stub()` to
    [`px_stub()`](https://statisticsgreenland.github.io/pxmake/reference/px_stub.px.md),
    `heading()` to
    [`px_heading()`](https://statisticsgreenland.github.io/pxmake/reference/px_heading.px.md),
    etc.
  - Change `pxsave()` to
    [`px_save()`](https://statisticsgreenland.github.io/pxmake/reference/px_save.md).
  - Change `micromake()` to
    [`px_micro()`](https://statisticsgreenland.github.io/pxmake/reference/px_micro.md).
- Remove functions
  - These functions have been deprecated and can no longer be used:
    `metamake()`,
    [`pxmake()`](https://statisticsgreenland.github.io/pxmake/reference/pxmake-package.md),
    `make_template()`.
- px-object changes
  - Change `x$codelists1` and `x$codelists2` to `x$cells1` and
    `x$cells2`.
  - Change `x$acrosscell` to `x$acrosscells`.
  - Change `x$variables1$type` to `x$variables1$variable-type`.
- Excel changes
  - Change sheet ‘Codelists1’ and ‘Codelists2’ to ‘Cells1’ and ‘Cells2’.
  - Change sheet ‘Acrosscell’ to ‘Acrosscells’.
  - In ‘Variables1’ sheet change column ‘type’ to ‘variable-type’ .
  - To set a variable as TIMEVAL add a new column ‘timeval’ in
    ‘Variables1’ and set it to ‘TRUE’. TIMEVAL can no longer be set with
    `type="TIME"` in Variables1.

### Breaking changes

- Rename ‘codelists’ to ‘cells’ in px-object and Excel workbooks.
  ([\#256](https://github.com/StatisticsGreenland/pxmake/issues/256))
- Deprecate ‘metamake’, ‘pxmake’ and ‘make_template’.
  ([\#198](https://github.com/StatisticsGreenland/pxmake/issues/198))
- Change name of all modifying functions by adding the prefix ‘px\_’.
  ([\#254](https://github.com/StatisticsGreenland/pxmake/issues/254))
- Rename ‘acrosscell’ to ‘acrosscells’.
  ([\#271](https://github.com/StatisticsGreenland/pxmake/issues/271))
- Rename `type` to `variable-type` in variables1.
  ([\#261](https://github.com/StatisticsGreenland/pxmake/issues/261))
- Move TIMEVAL to its own column in variables1, instead of having it as
  part of `variable-type`.
  ([\#265](https://github.com/StatisticsGreenland/pxmake/issues/265))
- Rename ‘pxsave’ to ‘px_save’.
  ([\#273](https://github.com/StatisticsGreenland/pxmake/issues/273))
- Rename ‘figures’ to ‘px_figures’, ‘order’ to ‘px_order’, ‘add_totals’
  to ‘px_add_totals’.
  ([\#274](https://github.com/StatisticsGreenland/pxmake/issues/274))
- Rename ‘micromake’ to ‘px_mircro’.

### New features

- Add modifying functions: `map()`, `baseperiod()`, `domain()`,
  `elimination()`, `descriptiondefault()`,
  [`order()`](https://rdrr.io/r/base/order.html), `precision()`,
  `cellnote()`, `cellnotex()`, `cfprices()`, `stockfa()`,
  `variable_type()`, `contvariable()`.
  ([\#246](https://github.com/StatisticsGreenland/pxmake/issues/246))
  ([\#125](https://github.com/StatisticsGreenland/pxmake/issues/125))
  ([\#223](https://github.com/StatisticsGreenland/pxmake/issues/223))
- `micromake()` use furrr package to run in parallel.
  ([\#248](https://github.com/StatisticsGreenland/pxmake/issues/248))
- `micromake()` removes headings if all figures are NA.
  ([\#250](https://github.com/StatisticsGreenland/pxmake/issues/250))

### Bug fixes and minor improvements

- Fix mistake in documentation for all table2 modifying functions.
- Avoid warning caused by helper function.
- Bugfix: `cellnote()` can handle an empty data frame.

## pxmake 0.10.1

### New features

- Change all modifying functions to also be getters, so e.g. `stub(x)`
  returns current STUB variables.
- `pxsave()` automatically adds values if they aren’t defined in
  codelists2.
- Add argument `save_data` to `pxsave()` to supress saving of ‘Data’
  sheet in Excel.
  ([\#204](https://github.com/StatisticsGreenland/pxmake/issues/204))
- Add arugment `data_path` to `pxsave()` to save data table as an .rds
  file.
  ([\#204](https://github.com/StatisticsGreenland/pxmake/issues/204))
- Add micromake arguments to control keywords for invidual tables.
  ([\#239](https://github.com/StatisticsGreenland/pxmake/issues/239))
- Add possibility for micromake to control output filenames.
  ([\#242](https://github.com/StatisticsGreenland/pxmake/issues/242))
- Add new modifying functions: `last_updated()`, `next_update()`,
  `language()`, `languages()`, `valuenote()`, `valuenotex()`,
  `contents()`, `description()`, `subject_area()`, `subject_code()`,
  [`title()`](https://rdrr.io/r/graphics/title.html),
  [`units()`](https://rdrr.io/r/base/units.html), `aggregallowed()`,
  `autopen()`, `axis_version()`, `codepage()`, `confidential()`,
  `copyright()`, `showdecimals()`, `tableid()`, `update-frequency()`,
  `contact()`, `link()`, `note()`, `notex()`, `infofile()`.

### Bug fixes and minor improvements

- Throw error if trying to remove mandatory keyword.
  ([\#208](https://github.com/StatisticsGreenland/pxmake/issues/208))
- Validate px after using modifying functions.
- validate_px checks that keywords in table1 and table2 are in the right
  table.
- Check that arguments to micromake are valid.
- Sort data before creating codelist from data frame.
  ([\#197](https://github.com/StatisticsGreenland/pxmake/issues/197))
- Add priority and complexity to list of unimplemented keywords.
- Add check if keywords are in correct table.
- Remove ‘micro’ prefix from filenames created by micromake().
- Use documentation functions for table1 and table2 functions, so they
  are and very easy to change.
- Add test coverage badge to README.md.
- Check that all defined variable-codes are in data.
- Add check that data columns are defined.
- In `micromake()` used preserve HEADING variables, and produces
  PX-files for all other variables.
- Bugfix: `heading()`, `stub()` and `figures()` no longer modify order
  of other variables.
  ([\#225](https://github.com/StatisticsGreenland/pxmake/issues/225))
- Remove duplication in documentaiton of pivot modifying functions.
- Bugfix: `last_updated()` created wrong keyword
  ([\#233](https://github.com/StatisticsGreenland/pxmake/issues/233))
- Remove ‘micro’ prefix from filenames created by micromake().
- Bugfix: wrong error message
  ([\#243](https://github.com/StatisticsGreenland/pxmake/issues/243))
- Bugfix: `last_updated()` modified wrong table.
- Bugfix: Remove NA values when creating PX-file
  ([\#205](https://github.com/StatisticsGreenland/pxmake/issues/205)).
- Bugfix: `px(input)` can be a path to an `.rds` file.

## pxmake 0.10.0

### New features

- The function
  [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
  creates a *px object*. This is a major change to the way package
  works. `pxmake` and `metamake` are soft deprecated. Use
  [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
  and `pxsave()` instead.
  ([\#197](https://github.com/StatisticsGreenland/pxmake/issues/197))
- To modify the px object, a number of *modifying functions* have been
  added: `charset()`, `creation_date()`, `decimals()`,
  [`matrix()`](https://rdrr.io/r/base/matrix.html), `stub()`,
  `heading()`, `timeval()`, `add_totals()` and `figures()`. Many more
  need to be added in the future, to cover all px keywords.

### Breaking changes

- The ‘rds’ version of the PX-file is completly gone. Neither
  [`pxmake()`](https://statisticsgreenland.github.io/pxmake/reference/pxmake-package.md)
  nor `metamake()` can create them. pxmake() and metamake() can convert
  between Excel and PX-files, and they return a px object invisibly.
  [`pxmake()`](https://statisticsgreenland.github.io/pxmake/reference/pxmake-package.md)
  and `metamake()` will be deprecated in the future.
- `make_template()` will be deprecated in the future -
  [`px()`](https://statisticsgreenland.github.io/pxmake/reference/px.md)
  automatically creates a metadata template if none is provided.
- `micromake()` now takes a px object as input.
- `add_totals()` now takes a px object as input.

### Bug fixes and minor improvements

- `README.md` has been rewritten, and includes a list of all keywords
  with modifying functions. All vignettes have been removed.
- `README.md` is now created from `README.Rmd` using
  [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html).

## pxmake 0.9.1

### New features

- Add `make_template` to create a minimial multilingual metadata
  template.
  ([\#186](https://github.com/StatisticsGreenland/pxmake/issues/186))
- Add `micromake` to create many small PX-files from a large data set.
  See `vignette(micromake)`.
  ([\#176](https://github.com/StatisticsGreenland/pxmake/issues/176))
- Support keyword VALUENOTE
  ([\#187](https://github.com/StatisticsGreenland/pxmake/issues/187))

### Bug fixes and minor improvements

- Add import of pipe. (Reverses commit \#d59866c)
- Add vignette about creating PX-file and multilingual PX-file.
- Fix edge case in list mergeing.
- Use latin1 to default encoding, but add CODEPAGE=utf-8.
- Set maximum column width in Excel.
- Add `create_data` argument to `metamake` that can supress generation
  of data.

## pxmake 0.9.0

### New features

- Let `metamake` accept a data frame as input and create a minimal
  metadata template for the data.

### Bug fixes and minor improvements

- Handle PX-files with either zero heading variables or zero stub
  variables.
  ([\#95](https://github.com/StatisticsGreenland/pxmake/issues/95))
- Fix edge case in list sorting caused by PX-files with no codes.
- Use values as codes if no code is given for add_totals variable.

## pxmake 0.8.1

### Bug fixes and minor improvements

- Use variable-code as variable-label if no label is given.
  ([\#173](https://github.com/StatisticsGreenland/pxmake/issues/173))
- Quote values of text keywords even if their text is a number.
  ([\#164](https://github.com/StatisticsGreenland/pxmake/issues/164))

## pxmake 0.8.0

### Breaking changes

- Split sheet ‘Table’ into two sheets by language dependency.
  ([\#65](https://github.com/StatisticsGreenland/pxmake/issues/65))
- Split column ‘position’ in ‘Variables’ sheet into ‘pivot’ and ‘order’.
  ([\#159](https://github.com/StatisticsGreenland/pxmake/issues/159))
- Check for illegal values in columns ‘pivot’ and ‘type’ in ’Variables
  sheet.
- Rename ‘variable’ and ‘long_name’ to ‘variable-code’ and
  ‘variable-label’.
  ([\#144](https://github.com/StatisticsGreenland/pxmake/issues/144))
- Add check for language-dependent mandatory variables in Excel
  metadata.

### New features

- Return rds object invisibly from `pxmake` and `metamake`.
  ([\#37](https://github.com/StatisticsGreenland/pxmake/issues/37))
- Support CONTVARIABLE.
  ([\#132](https://github.com/StatisticsGreenland/pxmake/issues/132))
- Preserve name of FIGURES variable through all data formats.

### Bug fixes and minor improvements

- Rename all internal functions from ‘source_data’ to ‘data’.
  ([\#151](https://github.com/StatisticsGreenland/pxmake/issues/151))
- Print error message without the name of the function that threw the
  error.
- Sort metadata before saving.

## pxmake 0.7.0

### New features

- Make `pxmake` and `metamake` read and write to `.rds` files.

### Bug fixes and minor improvements

- Validate arguments to `pxmake` and `metamake` and give helpful error
  messages.
- Support PX-files without LANGUAGES keyword.
  ([\#143](https://github.com/StatisticsGreenland/pxmake/issues/143))
- Remove dependencies between test and run them in parallel.
  ([\#112](https://github.com/StatisticsGreenland/pxmake/issues/112))
- Split tests into more files to run them faster in parallel.
  ([\#149](https://github.com/StatisticsGreenland/pxmake/issues/149))
- GitHub actions cancels build if a newer commit is pushed.
- Lots of documentation improvements.

## pxmake 0.6.0

### Breaking changes

- Rewrite `pxmake` and `metamake` to improve readability.

## pxmake 0.5.2

### Bug fixes and minor improvements

- Remove need for awkward quoting in metadata with multiple languages.
  Previously this has to be written as `en","da","kl`, but can now be
  `en,da,kl`.
- Remove VARIABLECODE for figures variables.
- Make `metamake` use value as code if code is missing.
- Ignore NAs when summing in `add_totals`.

## pxmake 0.5.1

### New features

- Add keyword VARIABLECODE which should variable names from source data.

### Bug fixes and minor improvements

- Remove quotes around YES in elimination.
- Make Excel table theme less dominant.
- Allow variable names to contain all characters except quotes (“).
- Improve installation instructions.

## pxmake 0.5.0

### Breaking changes

- Rename ‘General’ metadata sheet to ‘Table’.
  ([\#85](https://github.com/StatisticsGreenland/pxmake/issues/85))
- Rename argument `pxmake(source_data_path)` to `source_data`
  ([\#71](https://github.com/StatisticsGreenland/pxmake/issues/71))
- Rename argument `metamake(out_path)` to `xlsx_path`.
  ([\#106](https://github.com/StatisticsGreenland/pxmake/issues/106))

### New features

- Write and read files in encoding defined by keyword CODEPAGE.
  ([\#114](https://github.com/StatisticsGreenland/pxmake/issues/114),
  [\#115](https://github.com/StatisticsGreenland/pxmake/issues/115))
- Break lines in PX-files with values longer than 256 characters.
  ([\#113](https://github.com/StatisticsGreenland/pxmake/issues/113))
- Support other main languages than English.
  ([\#54](https://github.com/StatisticsGreenland/pxmake/issues/54))
- Support any language code (previously only supported: en, da, kl, and
  fo).
  ([\#117](https://github.com/StatisticsGreenland/pxmake/issues/117))

### Bug fixes and minor improvements

- Add formatting to Excel metadata workbook created by `metamake()`.
  ([\#100](https://github.com/StatisticsGreenland/pxmake/issues/100))
- Add argument `rds_data_path` to `metamake()` to save data as rds file.
  ([\#108](https://github.com/StatisticsGreenland/pxmake/issues/108))
- Let
  [`pxmake()`](https://statisticsgreenland.github.io/pxmake/reference/pxmake-package.md)
  accept data frame as source data in addition to accepting a path.
  ([\#71](https://github.com/StatisticsGreenland/pxmake/issues/71))
- Turn off [`readLines()`](https://rdrr.io/r/base/readLines.html)
  warning for missing EOL character.
  ([\#113](https://github.com/StatisticsGreenland/pxmake/issues/113))
- Rename `.figures` to `figures_` in metadata created by `metamake()`.
  ([\#104](https://github.com/StatisticsGreenland/pxmake/issues/104))
- Allow forward slashes in variables names.
- Let `metamake()` support tables without time variables.
  ([\#120](https://github.com/StatisticsGreenland/pxmake/issues/120))
- Use values as codes in there are no codes in metadata.
  ([\#102](https://github.com/StatisticsGreenland/pxmake/issues/102))

## pxmake 0.4.0

### New features

- Add metamake() which creates an Excel metadata workbook from a
  PX-file. This is the inverse function of pxmake().
  ([\#68](https://github.com/StatisticsGreenland/pxmake/issues/68))

### Bug fixes and minor improvements

- CODES added for time variables.
  ([\#91](https://github.com/StatisticsGreenland/pxmake/issues/91))
- Data cube is sorted correctly for data with more than one heading
  variable.
  ([\#93](https://github.com/StatisticsGreenland/pxmake/issues/93))
- Non-figure variables are read as character.
  ([\#87](https://github.com/StatisticsGreenland/pxmake/issues/87),
  [\#84](https://github.com/StatisticsGreenland/pxmake/issues/84),
  [\#73](https://github.com/StatisticsGreenland/pxmake/issues/73))
- Add keywords AUTOPEN and AGGREGALLOWED
- Allow figures variable to have any name (previously only ‘values’)

## pxmake 0.3.0

### New features

- Time values are added correctly the PX-file.
- If no `source_data_path` is given,
  [`pxmake()`](https://statisticsgreenland.github.io/pxmake/reference/pxmake-package.md)
  looks for a ‘Data’ sheet in the metadata and uses that instead.
- New argument: add_totals. Choose variable(s) to add a total level to.

### Bug fixes and minor improvements

- Complete data by using values both from codelist and data.
- Lot of small changes to avoid warnings when running R CMD check.
- Add automatic testing on GitHub.
- Move helper functions to own file.

## pxmake 0.2.0

### New features

- Better names for sheets, variables and keywords in Excel metadata
  workbook.
- Sort keywords in recommended order

### Bug fixes and minor improvements

- Add installation instruction to README.md

## pxmake 0.1.0

### New features

- Add support for more than one HEADING variable

### Bug fixes and minor improvements

- Allow blank fields in metadata (NA is replaced with ““)
- PxJob is run as parts of tests
