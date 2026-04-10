suppressPackageStartupMessages({
  library(pxmake)
  library(bench)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

options(width = 120)

benchmarks_dir <- "benchmarks"

save_benchmark_px <- function(dir, filename, n_municipality) {
  path <- file.path(dir, filename)

  if (file.exists(path)) {
    return(invisible(NULL))
  }

  message("Generating: ", filename)
  expand_grid(
    municipality   = paste0("municipality", seq_len(n_municipality)),
    gender         = paste0("gender", 1:3),
    age            = paste0("age", 1:100),
    residence_type = paste0("res", 1:11),
    time           = as.character(1975:1977)
  ) |>
    mutate(figures = round(runif(n()) * 1000)) |>
    px() |>
    px_save(path)
}

generate_benchmark_px_files <- function(dir) {
  save_benchmark_px(dir, "benchmark_100k_cells.px", n_municipality = 10)
  save_benchmark_px(dir, "benchmark_300k_cells.px", n_municipality = 30)
  save_benchmark_px(dir, "benchmark_1m_cells.px", n_municipality = 100)
  save_benchmark_px(dir, "benchmark_3m_cells.px", n_municipality = 300)
}

generate_benchmark_px_files(file.path(benchmarks_dir, "fixtures"))

benchmark_filenames <- c(
  "benchmark_100k_cells.px",
  "benchmark_300k_cells.px",
  "benchmark_1m_cells.px",
  "benchmark_3m_cells.px"
)

files <- file.path(benchmarks_dir, "fixtures", benchmark_filenames)

benchmark_file <- function(path) {
  message("Benchmarking: ", basename(path))
  x <- px(path)
  tmp <- tempfile(fileext = ".px")
  on.exit(unlink(tmp))

  bench::mark(
    px = px(path),
    px_save = px_save(x, tmp),
    check = FALSE
  ) |>
    mutate(
      file = basename(path),
      file_size_kb = round(file.size(path) / 1e3, 1)
    )
}

version <-
  tibble(
    pxmake_version = as.character(packageVersion("pxmake")),
    r_version      = R.version$version.string
  )

hardware <-
  tibble(
    os = Sys.info()[["sysname"]],
    github_arch = Sys.getenv("RUNNER_ARCH", unset = NA),
    cpu_model = if (file.exists("/proc/cpuinfo")) {
      system(
        "grep 'model name' /proc/cpuinfo | head -1 | cut -d: -f2 | xargs",
        intern = TRUE
      )
    } else {
      "unknown"
    },
    n_cores = parallel::detectCores(),
    benchmark_ms = round(as.numeric(
      bench::mark(sort(runif(1e6)), min_iterations = 5)$median
    ) * 1000) # s to ms
  )

results <-
  files |>
  map(benchmark_file) |>
  list_rbind() |>
  select(expression, file_size_kb, file, median, mem_alloc, n_itr, total_time) |>
  arrange_all()

output <- c(
  "\nVersion:",
  capture.output(glimpse(version)),
  "\nHardware:",
  capture.output(glimpse(hardware)),
  "\nResults:",
  capture.output(print(results))
)

cat(output, sep = "\n")

if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  writeLines(output, file.path(benchmarks_dir, "benchmark.txt"))
}
