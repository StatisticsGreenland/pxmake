# Compact representation of PX data cube
# --------------------------------------

new_px_data_compact <- function(dimensions, figures, figure_name) {
  stopifnot(is.list(dimensions))
  stopifnot(length(figure_name) == 1)

  structure(
    list(
      dimensions = dimensions,
      figures = figures,
      figure_name = figure_name
    ),
    class = c("px_data_compact", "list")
  )
}

is_px_data_compact <- function(x) {
  inherits(x, "px_data_compact")
}

nrow_px_data <- function(x) {
  if (is_px_data_compact(x)) {
    return(length(x$figures))
  }
  nrow(x)
}

materialize_px_data <- function(x) {
  if (!is_px_data_compact(x)) {
    return(x)
  }

  grid <- do.call(tidyr::expand_grid, x$dimensions)
  grid[[x$figure_name]] <- x$figures
  tibble::as_tibble(grid)
}

should_materialize_px_data <- function(grid_n, threshold = 5e7) {
  isTRUE(grid_n <= threshold)
}
