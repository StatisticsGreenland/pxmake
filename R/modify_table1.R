modify_table1 <- function(x, keyword, value) {
  x$table1 <- modify_or_add_row(x$table1, "keyword", keyword, "value", value)
  return(x)
}

#' @rdname charset.px
#' @export
charset <- function(x, ...) {
  UseMethod("charset")
}

#' Set CHARSET
#'
#' @export
charset.px <- function(x, value, validate = TRUE) {
  modify_table1(x, "CHARSET", value)
}


#' @rdname creation_date.px
#' @export
creation_date <- function(x, value) {
  UseMethod("creation_date")
}

#' Set CREATION-DATE
#'
#' @export
creation_date.px <- function(x, value = format(Sys.time(), "%Y%m%d %H:%M")) {
  modify_table1(x, "CREATION-DATE", value)
}


#' @rdname matrix.px
#' @export
matrix <- function(x, value) {
  UseMethod("matrix")
}

#' Set MATRIX
#'
#' @export
matrix.px <- function(x, value) {
  modify_table1(x, "MATRIX", value)
}


#' @rdname decimals.px
#' @export
decimals <- function(x, value) {
  UseMethod("decimals")
}

#' Set DECIMALS
#'
#' @export
decimals.px <- function(x, value) {
  modify_table1(x, "DECIMALS", value)
}




# # # Utility function to create a generic function
# # createGenericFunction <- function(name) {
# #   assign(name, function(x, value) {
# #     UseMethod(name)
# #   }, envir = globalenv())
# # }
# #
# # # Utility function to create an S3 method
# # createS3Method <- function(generic, class, impl) {
# #   method_name <- paste0(generic, ".", class)
# #   assign(method_name, function(x, value) impl(x, value), envir = globalenv())
# # }
# #
# # Common implementation function
#
# #
# # # Function to create both generic and its S3 method
# # createGenericAndMethod <- function(name, class, keyword) {
# #   createGenericFunction(name)
# #   createS3Method(name, class, function(x, value) modify_table1(x, keyword, value))
# # }
# #
# # # Example of creating functions
# # createGenericAndMethod("charset", "px", "CHARSET")
# # createGenericAndMethod("creation_date", "px", "CREATION-DATE")
# # createGenericAndMethod("matrix", "px", "MATRIX")
#
# # Function to create both generic and its S3 method with documentation
# createGenericAndMethod <- function(name, class, keyword) {
#   generic_function <- function(x, value) {
#     UseMethod(name)
#   }
#
#   # Create Roxygen documentation for the generic function
#   roxygen_generic <- paste0(
#     "#' @name ", name, "\n",
#     "#' @export\n",
#     "#' @description Generic function for ", name, "\n",
#     "#' @param x Object\n",
#     "#' @param value Value to set\n"
#   )
#
#   method_function <- function(x, value) {
#     modify_table1(x, keyword, value)
#   }
#
#   # Create Roxygen documentation for the method
#   roxygen_method <- paste0(
#     "#' @name ", name, ".", class, "\n",
#     "#' @export\n",
#     "#' @description Method for class '", class, "' for ", name, "\n",
#     "#' @param x Object of class '", class, "'\n",
#     "#' @param value Value to set\n"
#   )
#
#   # Assign functions and their documentation
#   assign(name, generic_function, envir = globalenv())
#   assign(paste0(name, ".", class), method_function, envir = globalenv())
#   assign(paste0("roxygen_docs_", name), roxygen_generic, envir = globalenv())
#   assign(paste0("roxygen_docs_", name, "_", class), roxygen_method, envir = globalenv())
# }
#
# # Example of creating functions with documentation
# createGenericAndMethod("charset", "px", "CHARSET")
# createGenericAndMethod("creation_date", "px", "CREATION-DATE")
# createGenericAndMethod("matrix", "px", "MATRIX")
