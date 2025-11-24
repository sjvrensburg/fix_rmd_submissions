# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`fix_rmd_submissions` is an R package designed to automatically fix common issues in student R Markdown submissions so they can knit successfully. The package addresses typical student errors like broken file paths and chunks that fail to evaluate.

**Current Status**: Early development - package structure initialized but no functions in `R/` directory yet. Example implementation exists in `agents/example_code_from_grok.R`.

## R Package Structure

Standard R package layout:
- `DESCRIPTION`: Package metadata (author: Stéfan Janse van Rensburg)
- `NAMESPACE`: Package namespace (currently empty)
- `R/`: Package functions (currently empty - needs to be populated)
- `agents/`: Example code and notes for development (not part of package, excluded in .gitignore)
- `real_examples/`: Real student submissions for testing (contains PII, excluded in .gitignore)

## Development Commands

### Build and Check
```r
# Load package for development
devtools::load_all()

# Document functions (after adding roxygen2 comments)
devtools::document()

# Check package
devtools::check()

# Install package locally
devtools::install()

# Build package
devtools::build()
```

### Testing
No test infrastructure set up yet. To add:
```r
usethis::use_testthat()
```

## Core Functionality

The main functionality (currently in `agents/example_code_from_grok.R`) involves two primary functions that need to be moved to `R/` and properly documented:

### `fix_student_rmd()`
Fixes individual R Markdown files by:
1. Sequentially evaluating chunks in a shared environment
2. Automatically adding `eval = FALSE` to chunks that fail
3. Converting bare file paths to `here::here("data", "filename")` format
4. Creating backups before modification

Key parameters:
- `input_rmd`: Path to student's .Rmd file
- `output_rmd`: Output path (defaults to `*_FIXED.Rmd`)
- `backup`: Create backup (default TRUE)
- `use_here`: Wrap bare paths with `here::here()` (default TRUE)
- `here_folder`: Subfolder for data files (default "data")

### `fix_all_in_folder()`
Batch processes all .Rmd files in a directory using `fix_student_rmd()`.

## Technical Details

### Path Fixing Strategy
The `.fix_paths_in_line()` helper uses regex to identify common data import functions (read_csv, read.csv, readRDS, fread, etc.) and wraps bare filenames with `here::here()`. It:
- Skips full-line comments
- Only targets filenames without path separators (/, \)
- Preserves absolute paths and URLs
- Maintains code structure (plots, strings, variable names untouched)

### Chunk Evaluation
- Uses persistent `eval_env` so successful chunks build on each other
- Evaluates chunks sequentially to maintain proper context
- Handles unnamed chunks, weird spacing, and inline R code
- Muffles warnings but captures errors

## Development Workflow

1. Move functions from `agents/example_code_from_grok.R` to `R/` directory
2. Add roxygen2 documentation to each function
3. Run `devtools::document()` to generate .Rd files
4. Add tests using testthat framework
5. Update DESCRIPTION with proper title, description, and dependencies
6. Choose license using `usethis::use_mit_license()` or `usethis::use_gpl3_license()`

## Dependencies

Current implicit dependencies to add to DESCRIPTION:
- `here` (for path fixing functionality)
- Suggested: `devtools`, `roxygen2`, `testthat` (for development)

## Privacy and Data Handling

- `real_examples/` and `agents/` are git-ignored to protect student PII
- Never commit files from these directories
- Use these for local testing only
