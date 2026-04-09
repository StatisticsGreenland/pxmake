suppressPackageStartupMessages({
  library(pxmake)
  library(bench)
  library(dplyr)
  library(purrr)
})

files <- list.files(file.path("benchmarks", "fixtures"), full.names = TRUE)

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

version_txt <- c(
  paste("pxmake", as.character(packageVersion("pxmake")), sep = "\t"),
  paste("R", R.version$version, sep = "\t")
)

hardware_txt <- c(
  paste("os", Sys.info()[["sysname"]], sep = "\t"),
  paste("github_arch", Sys.getenv("RUNNER_ARCH", unset = NA), sep = "\t"),
  paste("cpu_model", if (file.exists("/proc/cpuinfo")) {
    system("grep 'model name' /proc/cpuinfo | head -1 | cut -d: -f2 | xargs", intern = TRUE)
  } else {
    "unknown"
  }, sep = "\t"),
  paste("n_cores", parallel::detectCores(), sep = "\t"),
  paste("benchmark_ms", round(as.numeric(
    bench::mark(sort(runif(1e6)), min_iterations = 5)$median
  ) * 1000), sep = "\t")
)

results <-
  files |>
  map(benchmark_file) |>
  list_rbind() |>
  select(expression, file_size_kb, file, median, mem_alloc, n_itr, total_time) |>
  arrange_all()

message("\nVersion:")
cat(paste0("  ", version_txt), sep = "\n")

message("\nHardware:")
cat(paste0("  ", hardware_txt), sep = "\n")

message("\nResults:")
print(results)
