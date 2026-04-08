# Load the helper files in a fixed dependency order so the app and tests can
# source a single entry point while the implementation stays split by concern.
source_paths <- vapply(
  sys.frames(),
  function(frame) frame$ofile %||% NA_character_,
  character(1)
)
source_paths <- source_paths[!is.na(source_paths)]
helper_path <- if (length(source_paths) > 0) {
  tail(source_paths, n = 1)
} else {
  "R/forecast_utils.R"
}

helper_dir <- dirname(normalizePath(helper_path, mustWork = TRUE))
helper_files <- c(
  "00-formatting-utils.R",
  "01-time-series-utils.R",
  "02-holiday-utils.R",
  "03-upload-and-parse-utils.R",
  "04-forecast-and-comparison-utils.R",
  "05-ui-text-utils.R"
)

helper_env <- environment()

invisible(lapply(
  helper_files,
  function(helper_file) {
    source(file.path(helper_dir, helper_file), local = helper_env)
  }
))
