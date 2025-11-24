# Path fixing utilities
# Internal helper functions for intelligently fixing bare file paths

#' Fix bare file paths in a line of R code
#'
#' @description
#' Intelligently replaces bare filenames in common data import functions
#' with `here::here()` wrapped paths. Only modifies actual file references,
#' never comments, strings outside import calls, or existing path references.
#'
#' @param line Character string representing a line of R code
#' @param data_folder Character string for the data subfolder (e.g., "data")
#'
#' @return Modified line with bare paths wrapped in here::here()
#'
#' @keywords internal
#' @noRd
fix_paths_in_line <- function(line, data_folder) {
  # Skip full-line comments
  if (grepl("^\\s*#", line)) {
    return(line)
  }

  # Skip lines that already use here::here()
  if (grepl("here::here\\(", line)) {
    return(line)
  }

  # Skip lines with absolute paths or relative path indicators
  if (grepl("[\"'](/|~/|\\.\\./)|\\.{2}\\\\", line)) {
    return(line)
  }

  # Common student import patterns (both tidyverse and base R)
  # This pattern matches common read functions followed by opening parenthesis
  import_functions <- c(
    # readr and tidyverse
    "read_csv", "read_tsv", "read_delim", "read_table", "read_fwf",
    # base R
    "read\\.csv", "read\\.csv2", "read\\.table", "read\\.delim", "read\\.delim2",
    "readRDS", "load", "source",
    # readxl
    "read_excel", "read_xlsx", "read_xls",
    # data.table
    "fread",
    # vroom
    "vroom",
    # qs
    "qread"
  )

  pattern <- paste0(
    "\\b(?:", paste(import_functions, collapse = "|"), ")\\s*\\(\\s*"
  )

  # Match: function("filename.ext") where filename has no path separators
  # Replace with: function(here::here("data_folder", "filename.ext"))
  result <- gsub(
    pattern = paste0("(", pattern, ")[\"']([^/\\'\"\\\\]+\\.[A-Za-z0-9]+)[\"']"),
    replacement = sprintf("\\1here::here(\"%s\", \"\\2\")", data_folder),
    x = line,
    perl = TRUE
  )

  return(result)
}


#' Check if 'here' package is available
#'
#' @description
#' Checks if the 'here' package is installed and loads it if needed.
#' Provides helpful error message if not available.
#'
#' @return Logical indicating success
#'
#' @keywords internal
#' @noRd
ensure_here_available <- function() {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop(
      "The 'here' package is required for path fixing but is not installed.\n",
      "Install it with: install.packages(\"here\")\n",
      "Or disable path fixing with: fix_paths = FALSE",
      call. = FALSE
    )
  }

  # Load here package quietly
  suppressPackageStartupMessages(
    requireNamespace("here", quietly = TRUE)
  )

  return(TRUE)
}
