## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

There's one \dontrun example (in R/classification.R). The example is not run
because it uses paths that does not exist. The purpose of the example is to
show which combination of arguments can be used in the function.

All URLs mentioned in the error '(possibly) invalid URLs' are valid. The error 
is probably caused because the URLs contain a lot of special characters. These
characters are necessary because they link to specific headings in a pdf 
document.
