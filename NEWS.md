# pxmake (development version)

## Bug fixes and minor improvements
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
