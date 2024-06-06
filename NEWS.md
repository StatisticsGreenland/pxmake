# pxmake 0.11.0

This release contains *a lot* of breaking changes. This was done because a few
functions had name clashes with popular packages. The prefix 'px_' was added to
almost all functions to avoid this.

Because of this massive breaking change, a few other breaking changes were made
as well, to create more intuitive naming and make the package more consistent.

## How to update code from before version 0.11.0
- Rename functions
  - Change all modifying functions by adding the prefix `px_`. E.g. change 
`stub()` to `px_stub()`, `heading()` to `px_heading()`, etc.
  - Change `pxsave()` to `px_save()`.
  - Change `micromake()` to `px_micro()`.
- Remove functions
  - These functions have been deprecated and can no longer be used: 
  `metamake()`, `pxmake()`, `make_template()`.
- px-object changes
  - Change `x$codelists1` and `x$codelists2` to `x$cells1` and `x$cells2`.
  - Change `x$acrosscell` to `x$acrosscells`.
  - Change `x$variables1$type` to `x$variables1$variable-type`.
- Excel changes
  - Change sheet 'Codelists1' and 'Codelists2' to 'Cells1' and 'Cells2'.
  - Change sheet 'Acrosscell' to 'Acrosscells'.
  - In 'Variables1' sheet change column 'type' to 'variable-type' .
  - To set a variable as TIMEVAL add a new column 'timeval' in 'Variables1' and 
  set it to 'TRUE'. TIMEVAL can no longer be set with `type="TIME"` in 
  Variables1.

## Breaking changes
- Rename 'codelists' to 'cells' in px-object and Excel workbooks. (#256)
- Deprecate 'metamake', 'pxmake' and 'make_template'. (#198)
- Change name of all modifying functions by adding the prefix 'px_'. (#254)
- Rename 'acrosscell' to 'acrosscells'. (#271)
- Rename `type` to `variable-type` in variables1. (#261)
- Move TIMEVAL to its own column in variables1, instead of having it as part of
`variable-type`. (#265)
- Rename 'pxsave' to 'px_save'. (#273)
- Rename 'figures' to 'px_figures', 'order' to 'px_order', 'add_totals' to 
'px_add_totals'. (#274)
- Rename 'micromake' to 'px_mircro'.

## New features
- Add modifying functions: `map()`, `baseperiod()`, `domain()`, `elimination()`,
`descriptiondefault()`, `order()`, `precision()`, `cellnote()`, `cellnotex()`,
`cfprices()`, `stockfa()`, `variable_type()`, `contvariable()`. (#246) (#125)
(#223)
- `micromake()` use furrr package to run in parallel. (#248)
- `micromake()` removes headings if all figures are NA. (#250)

## Bug fixes and minor improvements
- Fix mistake in documentation for all table2 modifying functions.
- Avoid warning caused by helper function.
- Bugfix: `cellnote()` can handle an empty data frame.

# pxmake 0.10.1

## New features
- Change all modifying functions to also be getters, so e.g. `stub(x)` returns
current STUB variables.
- `pxsave()` automatically adds values if they aren't defined in codelists2.
- Add argument `save_data` to `pxsave()` to supress saving of 'Data' sheet in 
Excel. (#204)
- Add arugment `data_path` to `pxsave()` to save data table as an .rds file. (#204)
- Add micromake arguments to control keywords for invidual tables. (#239)
- Add possibility for micromake to control output filenames. (#242)
- Add new modifying functions: `last_updated()`, `next_update()`, `language()`,
`languages()`, `valuenote()`, `valuenotex()`, `contents()`, `description()`,
`subject_area()`, `subject_code()`, `title()`, `units()`, `aggregallowed()`, 
`autopen()`, `axis_version()`, `codepage()`, `confidential()`, `copyright()`,
`showdecimals()`, `tableid()`, `update-frequency()`, `contact()`, `link()`,
`note()`, `notex()`, `infofile()`.

## Bug fixes and minor improvements
- Throw error if trying to remove mandatory keyword. (#208)
- Validate px after using modifying functions.
- validate_px checks that keywords in table1 and table2 are in the right table.
- Check that arguments to micromake are valid.
- Sort data before creating codelist from data frame. (#197)
- Add priority and complexity to list of unimplemented keywords.
- Add check if keywords are in correct table.
- Remove 'micro' prefix from filenames created by micromake(). 
- Use documentation functions for table1 and table2 functions, so they are
and very easy to change.
- Add test coverage badge to README.md.
- Check that all defined variable-codes are in data.
- Add check that data columns are defined.
- In `micromake()` used preserve HEADING variables, and produces px files for
all other variables.
- Bugfix: `heading()`, `stub()` and `figures()` no longer modify order of 
other variables. (#225)
- Remove duplication in documentaiton of pivot modifying functions.
- Bugfix: `last_updated()` created wrong keyword (#233)
- Remove 'micro' prefix from filenames created by micromake(). 
- Bugfix: wrong error message (#243)
- Bugfix: `last_updated()` modified wrong table.
- Bugfix: Remove NA values when creating px file (#205).
- Bugfix: `px(input)` can be a path to an `.rds` file.

# pxmake 0.10.0

## New features
- The function `px()` creates a *px object*. This is a major change to the way
package works. `pxmake` and `metamake` are soft deprecated. Use `px()` and `pxsave()` instead. (#197)
- To modify the px object, a number of *modifying functions* have been added:
 `charset()`, `creation_date()`, `decimals()`, `matrix()`, `stub()`, `heading()`, 
`timeval()`, `add_totals()` and `figures()`. Many more need to be added in the
future, to cover all px keywords.

## Breaking changes
- The 'rds' version of the px file is completly gone. Neither `pxmake()` nor 
`metamake()` can create them. pxmake() and metamake() can convert between Excel
and px files, and they return a px object invisibly. `pxmake()` and `metamake()`
will be deprecated in the future.
- `make_template()` will be deprecated in the future - `px()` automatically
creates a metadata template if none is provided.
- `micromake()` now takes a px object as input.
- `add_totals()` now takes a px object as input.

## Bug fixes and minor improvements
- `README.md` has been rewritten, and includes a list of all keywords with
modifying functions. All vignettes have been removed.
- `README.md` is now created from `README.Rmd` using `knitr::knit()`.

# pxmake 0.9.1

## New features
- Add `make_template` to create a minimial multilingual metadata template. (#186)
- Add `micromake` to create many small px-files from a large data set. See
`vignette(micromake)`. (#176)
- Support keyword VALUENOTE (#187)

## Bug fixes and minor improvements
- Add import of pipe. (Reverses commit #d59866c)
- Add vignette about creating px file and multilingual px file.
- Fix edge case in list mergeing.
- Use latin1 to default encoding, but add CODEPAGE=utf-8.
- Set maximum column width in Excel.
- Add `create_data` argument to `metamake` that can supress generation of data.

# pxmake 0.9.0

## New features
- Let `metamake` accept a data frame as input and create a minimal metadata 
template for the data.

## Bug fixes and minor improvements
- Handle px-files with either zero heading variables or zero stub variables. (#95)
- Fix edge case in list sorting caused by px-files with no codes.
- Use values as codes if no code is given for add_totals variable.

# pxmake 0.8.1

## Bug fixes and minor improvements
- Use variable-code as variable-label if no label is given. (#173)
- Quote values of text keywords even if their text is a number. (#164)

# pxmake 0.8.0

## Breaking changes
- Split sheet 'Table' into two sheets by language dependency. (#65)
- Split column 'position' in 'Variables' sheet into 'pivot' and 'order'. (#159)
- Check for illegal values in columns 'pivot' and 'type' in 'Variables sheet.
- Rename 'variable' and 'long_name' to 'variable-code' and 'variable-label'. (#144)
- Add check for language-dependent mandatory variables in Excel metadata.

## New features
- Return rds object invisibly from `pxmake` and `metamake`. (#37)
- Support CONTVARIABLE. (#132)
- Preserve name of FIGURES variable through all data formats.

## Bug fixes and minor improvements
- Rename all internal functions from 'source_data' to 'data'. (#151)
- Print error message without the name of the function that threw the error.
- Sort metadata before saving.

# pxmake 0.7.0

## New features
- Make `pxmake` and `metamake` read and write to `.rds` files.

## Bug fixes and minor improvements
- Validate arguments to `pxmake` and `metamake` and give helpful error messages.
- Support px-files without LANGUAGES keyword. (#143)
- Remove dependencies between test and run them in parallel. (#112)
- Split tests into more files to run them faster in parallel. (#149) 
- GitHub actions cancels build if a newer commit is pushed.
- Lots of documentation improvements.

# pxmake 0.6.0

## Breaking changes
- Rewrite `pxmake` and `metamake` to improve readability.

# pxmake 0.5.2

## Bug fixes and minor improvements
- Remove need for awkward quoting in metadata with multiple languages.
Previously this has to be written as `en","da","kl`, but can now be `en,da,kl`.
- Remove VARIABLECODE for figures variables.
- Make `metamake` use value as code if code is missing.
- Ignore NAs when summing in `add_totals`.

# pxmake 0.5.1

## New features

- Add keyword VARIABLECODE which should variable names from source data.

## Bug fixes and minor improvements

- Remove quotes around YES in elimination.
- Make Excel table theme less dominant.
- Allow variable names to contain all characters except quotes (").
- Improve installation instructions.

# pxmake 0.5.0

## Breaking changes

- Rename 'General' metadata sheet to 'Table'. (#85)
- Rename argument `pxmake(source_data_path)` to `source_data` (#71) 
- Rename argument `metamake(out_path)` to `xlsx_path`. (#106)

## New features

- Write and read files in encoding defined by keyword CODEPAGE. (#114, #115)
- Break lines in px-files with values longer than 256 characters. (#113)
- Support other main languages than English. (#54)
- Support any language code (previously only supported: en, da, kl, and fo). (#117)


## Bug fixes and minor improvements

- Add formatting to Excel metadata workbook created by `metamake()`. (#100)
- Add argument `rds_data_path` to `metamake()` to save data as rds file. (#108)
- Let `pxmake()` accept data frame as source data in addition to accepting a
path. (#71)
- Turn off `readLines()` warning for missing EOL character. (#113)
- Rename `.figures` to `figures_` in metadata created by `metamake()`. (#104)
- Allow forward slashes in variables names.
- Let `metamake()` support tables without time variables. (#120)
- Use values as codes in there are no codes in metadata. (#102)


# pxmake 0.4.0

## New features

- Add metamake() which creates an Excel metadata workbook from a px-file. This
is the inverse function of pxmake(). (#68)

## Bug fixes and minor improvements

- CODES added for time variables. (#91)
- Data cube is sorted correctly for data with more than one heading variable. (#93)
- Non-figure variables are read as character. (#87, #84, #73)
- Add keywords AUTOPEN and AGGREGALLOWED
- Allow figures variable to have any name (previously only 'values')

# pxmake 0.3.0

## New features

- Time values are added correctly the px file.
- If no `source_data_path` is given, `pxmake()` looks for a 'Data' sheet in the metadata and uses that instead.
- New argument: add_totals. Choose variable(s) to add a total level to.

## Bug fixes and minor improvements

- Complete data by using values both from codelist and data.
- Lot of small changes to avoid warnings when running R CMD check.
- Add automatic testing on GitHub.
- Move helper functions to own file.

# pxmake 0.2.0

## New features

- Better names for sheets, variables and keywords in Excel metadata workbook.
- Sort keywords in recommended order

## Bug fixes and minor improvements

- Add installation instruction to README.md

# pxmake 0.1.0

## New features

- Add support for more than one HEADING variable

## Bug fixes and minor improvements

- Allow blank fields in metadata (NA is replaced with "")
- PxJob is run as parts of tests
