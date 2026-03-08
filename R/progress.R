# Progress helpers for large PX read/write operations

new_px_progress <- function(total = 100, enabled = interactive()) {
  if (!enabled) {
    return(NULL)
  }

  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)

  list(
    tick = function(value, label = NULL) {
      utils::setTxtProgressBar(pb, value)
      if (!is.null(label)) {
        message(label)
      }
    },
    close = function() {
      try(close(pb), silent = TRUE)
    }
  )
}

is_large_px_dataset <- function(n_values, threshold = 1e6) {
  isTRUE(!is.na(n_values) && n_values >= threshold)
}

tick_px_progress_10 <- function(prog, i, n, prefix = NULL) {
  if (is.null(prog) || n <= 0) {
    return(invisible(NULL))
  }

  pct <- floor((i / n) * 100)
  pct10 <- min(100, (pct %/% 10) * 10)

  last_pct <- get0(".last_pct10", envir = environment(prog$tick), inherits = FALSE)
  if (is.null(last_pct) || pct10 > last_pct) {
    assign(".last_pct10", pct10, envir = environment(prog$tick))
    if (is.null(prefix)) {
      prog$tick(pct10)
    } else {
      prog$tick(pct10, paste0(prefix, " ", pct10, "%"))
    }
  }

  invisible(NULL)
}
