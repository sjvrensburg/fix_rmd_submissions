#' Run Interactive Package Demonstration
#'
#' @description
#' Runs a comprehensive interactive demonstration of all fixrmdsubmissions
#' features in a temporary directory. The demo showcases:
#'
#' - Path fixing with here::here()
#' - Chunk evaluation and selective disabling
#' - Output limiting to prevent massive data dumps
#' - Student identification (add_student_info)
#' - Batch folder processing
#' - Knitting fixed files to HTML/PDF
#' - Backup creation
#' - Transparency comments
#'
#' @param demo_type Character string specifying which demo to run:
#'   - "interactive" (default): Full interactive demo with prompts and pauses
#'   - "quick": Quick non-interactive demo showing all features
#'   - "custom": Run with custom settings (see other parameters)
#'
#' @param keep_files Logical. If TRUE, demo files are kept in a subdirectory
#'   of the current working directory instead of being deleted. Default is FALSE
#'   (files created in temp directory and cleaned up).
#'
#' @param output_dir Character string. If keep_files = TRUE, this specifies
#'   where to create the demo folder. Default is "fixrmdsubmissions_demo" in
#'   the current directory.
#'
#' @param auto_advance Logical. If TRUE (default), demo automatically advances
#'   through steps. If FALSE, waits for user to press Enter between steps.
#'   Only applies when demo_type = "interactive".
#'
#' @param knit_outputs Logical. If TRUE, attempts to knit the fixed files to
#'   HTML (requires rmarkdown and pandoc). Default is TRUE.
#'
#' @return Invisibly returns a list with:
#'   - demo_dir: Path to demo directory
#'   - files_created: Character vector of created files
#'   - fix_results: Results from fix_folder()
#'   - knit_results: Results from knit_fixed_files() (if knit_outputs = TRUE)
#'
#' @examples
#' \dontrun{
#' # Run full interactive demo (recommended for first-time users)
#' run_demo()
#'
#' # Quick non-interactive demo
#' run_demo("quick")
#'
#' # Keep demo files for inspection
#' run_demo(keep_files = TRUE, output_dir = "my_demo")
#'
#' # Run custom demo with manual advancement
#' run_demo("interactive", auto_advance = FALSE)
#'
#' # Just fix files, don't knit
#' run_demo(knit_outputs = FALSE)
#' }
#'
#' @importFrom utils write.csv
#'
#' @export
run_demo <- function(demo_type = c("interactive", "quick", "custom"),
                     keep_files = FALSE,
                     output_dir = "fixrmdsubmissions_demo",
                     auto_advance = TRUE,
                     knit_outputs = TRUE) {

  demo_type <- match.arg(demo_type)

  # Determine if we should pause for user input
  pause_enabled <- (demo_type == "interactive" && !auto_advance)

  # Helper function for pausing
  pause_for_user <- function(message = "Press [Enter] to continue...") {
    if (pause_enabled) {
      cat("\n")
      readline(prompt = message)
    } else if (demo_type == "interactive") {
      Sys.sleep(1.5)  # Brief pause for reading
    }
  }

  # Helper function for section headers
  print_section <- function(title, char = "=") {
    width <- 70
    cat("\n")
    cat(strrep(char, width), "\n")
    cat(title, "\n")
    cat(strrep(char, width), "\n\n")
  }

  # Setup demo directory
  if (keep_files) {
    demo_dir <- file.path(getwd(), output_dir)
    if (dir.exists(demo_dir)) {
      warning("Demo directory already exists: ", demo_dir,
              "\nFiles may be overwritten.")
    }
    dir.create(demo_dir, showWarnings = FALSE, recursive = TRUE)
  } else {
    demo_dir <- tempfile(pattern = "fixrmdsubmissions_demo_")
    dir.create(demo_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Store original directory
  orig_dir <- getwd()
  on.exit(setwd(orig_dir), add = TRUE)

  # Change to demo directory
  setwd(demo_dir)

  # Print welcome message
  if (demo_type != "quick") {
    print_section("fixrmdsubmissions Package - Comprehensive Demonstration")
    cat("This demo showcases all package features with realistic student submissions.\n\n")
    cat("Demo location:", demo_dir, "\n")

    if (keep_files) {
      cat("\nFiles will be kept after demo for your inspection.\n")
    } else {
      cat("\nFiles will be created in a temporary directory and cleaned up.\n")
    }

    pause_for_user()
  }

  # Create realistic folder structure
  if (demo_type != "quick") {
    print_section("Step 1: Creating Realistic Folder Structure")
    cat("Simulating a typical LMS download with student submissions...\n\n")
  }

  # Create student folders
  student_folders <- c(
    "Smith_John_12345",
    "Doe_Jane_67890",
    "Johnson_Mike_11111"
  )

  for (folder in student_folders) {
    dir.create(folder, showWarnings = FALSE)
  }

  # Create data files at root level
  data_files <- create_demo_data_files(demo_dir)

  if (demo_type != "quick") {
    cat("Created folder structure:\n")
    cat("  ./                          <- Project root (data files here)\n")
    for (folder in student_folders) {
      cat("  ./", folder, "/\n", sep = "")
      cat("      |-- homework.Rmd      <- Student submission\n")
    }
    cat("\nData files created:\n")
    for (f in data_files) {
      cat("  -", basename(f), "\n")
    }

    pause_for_user()
  }

  # Copy demo files to student folders
  extdata_dir <- system.file("extdata", package = "fixrmdsubmissions")

  # Different demo file for each student
  demo_files <- c(
    "demo_broken_submission.Rmd",
    "demo_more_issues.Rmd",
    "demo_minimal_issues.Rmd"
  )

  for (i in seq_along(student_folders)) {
    src_file <- file.path(extdata_dir, demo_files[i])
    if (file.exists(src_file)) {
      file.copy(src_file,
                file.path(student_folders[i], "homework.Rmd"),
                overwrite = TRUE)
    }
  }

  # Create .here file
  if (demo_type != "quick") {
    print_section("Step 2: Marking Project Root with .here File")
    cat("Creating .here file to mark project root for here::here() paths...\n\n")
  }

  writeLines("here root", ".here")

  if (demo_type != "quick") {
    cat("[OK] .here file created\n")
    cat("\nThis tells the 'here' package where to look for data files.\n")
    pause_for_user()
  }

  # Run fix_folder with all features
  if (demo_type != "quick") {
    print_section("Step 3: Fixing All Submissions with fix_folder()")
    cat("Running fix_folder() with key features:\n")
    cat("  - fix_paths = TRUE         (convert to here::here())\n")
    cat("  - data_folder = '.'        (data at root level)\n")
    cat("  - limit_output = TRUE      (prevent massive data dumps)\n")
    cat("  - add_student_info = TRUE  (add folder names as headings)\n")
    cat("  - backup = TRUE            (create .bak files)\n\n")

    pause_for_user()
  }

  fix_results <- fix_folder(
    path = ".",
    recursive = TRUE,
    fix_paths = TRUE,
    data_folder = ".",
    limit_output = TRUE,
    add_student_info = TRUE,
    quiet = (demo_type == "quick")
  )

  if (demo_type != "quick") {
    cat("\n[OK] All files processed successfully!\n")
    cat("\nFiles created:\n")
    fixed_files <- list.files(".", pattern = "_FIXED\\.Rmd$",
                             recursive = TRUE, full.names = TRUE)
    backup_files <- list.files(".", pattern = "\\.Rmd\\.bak$",
                              recursive = TRUE, full.names = TRUE)

    for (f in fixed_files) {
      cat("  -", f, "\n")
    }
    cat("\nBackups created:\n")
    for (f in backup_files) {
      cat("  -", f, "\n")
    }

    pause_for_user()
  }

  # Show example of fixes
  if (demo_type == "interactive") {
    print_section("Step 4: Examining the Fixes")
    cat("Let's look at what changed in one file...\n\n")

    example_file <- file.path(student_folders[1], "homework_FIXED.Rmd")
    if (file.exists(example_file)) {
      lines <- readLines(example_file, n = 50)

      cat("First 50 lines of", student_folders[1], "/homework_FIXED.Rmd:\n")
      cat(strrep("-", 70), "\n")
      cat(paste(lines, collapse = "\n"), "\n")
      cat(strrep("-", 70), "\n\n")

      cat("Notice:\n")
      cat("  1. Setup chunk injected with library(pander) and output limits\n")
      cat("  2. Student folder name added as heading (", student_folders[1], ")\n", sep = "")
      cat("  3. Transparency comments like [fixrmdsubmissions] added\n")
      cat("  4. Paths converted to here::here() format\n")
      cat("  5. Failing chunks marked with eval = FALSE\n")

      pause_for_user()
    }
  }

  # Knit the files
  knit_results <- NULL
  if (knit_outputs) {
    if (demo_type != "quick") {
      print_section("Step 5: Knitting Fixed Files to HTML")
      cat("Running knit_fixed_files() to generate HTML outputs...\n\n")

      pause_for_user()
    }

    tryCatch({
      knit_results <- knit_fixed_files(
        path = ".",
        recursive = TRUE,
        quiet = (demo_type == "quick")
      )

      if (demo_type != "quick") {
        cat("\n[OK] Files knitted successfully!\n")

        html_files <- list.files(".", pattern = "_FIXED\\.html$",
                                recursive = TRUE, full.names = TRUE)
        if (length(html_files) > 0) {
          cat("\nHTML outputs created:\n")
          for (f in html_files) {
            size <- file.info(f)$size
            size_kb <- round(size / 1024, 1)
            cat("  -", f, sprintf("(%.1f KB)", size_kb), "\n")
          }
        }
      }
    }, error = function(e) {
      if (demo_type != "quick") {
        cat("\nNote: Knitting skipped (requires rmarkdown and pandoc)\n")
        cat("Error:", conditionMessage(e), "\n")
      }
    })

    if (demo_type != "quick") {
      pause_for_user()
    }
  }

  # Summary
  if (demo_type != "quick") {
    print_section("Demonstration Complete!", char = "=")

    cat("Summary of what was demonstrated:\n\n")

    cat("[OK] Batch Processing:        fix_folder() processed", length(student_folders), "submissions\n")
    cat("[OK] Path Fixing:             Bare paths converted to here::here()\n")
    cat("[OK] Output Limiting:         Setup chunk injected with pander options\n")
    cat("[OK] Student Identification:  Folder names added as headings\n")
    cat("[OK] Error Handling:          Failing chunks marked eval = FALSE\n")
    cat("[OK] Transparency:            All changes documented with comments\n")
    cat("[OK] Backups:                 Original files saved as .bak\n")
    if (!is.null(knit_results)) {
      cat("[OK] Knitting:                HTML files generated from fixed submissions\n")
    }

    cat("\n")
    cat(strrep("=", 70), "\n\n")

    if (keep_files) {
      cat("Demo files saved in:", demo_dir, "\n")
      cat("\nYou can:\n")
      cat("  - Examine the _FIXED.Rmd files to see changes\n")
      cat("  - Compare with .bak files to see originals\n")
      cat("  - Open .html files to see knitted outputs\n")
      cat("  - Review transparency comments in the fixed files\n\n")
    } else {
      cat("Demo ran in temporary directory (files will be cleaned up).\n")
      cat("Run with keep_files = TRUE to inspect the generated files.\n\n")
    }

    cat("To use the package with your own submissions:\n")
    cat("  1. Put all student folders in one directory\n")
    cat("  2. Put data files at the root level (or in a 'data' folder)\n")
    cat("  3. Run: writeLines('here root', '.here')\n")
    cat("  4. Run: fix_folder('.', data_folder = '.')\n")
    cat("  5. Run: knit_fixed_files('.')\n\n")
  }

  # Collect results
  all_files <- list.files(demo_dir, recursive = TRUE, full.names = TRUE)

  results <- list(
    demo_dir = demo_dir,
    files_created = all_files,
    fix_results = fix_results,
    knit_results = knit_results
  )

  invisible(results)
}


#' Create Demo Data Files
#'
#' @description
#' Internal helper function to create realistic data files for the demo.
#' Creates CSV files with student data that the demo R Markdown files
#' will attempt to load.
#'
#' @param demo_dir Path to demo directory
#'
#' @return Character vector of created file paths
#'
#' @keywords internal
create_demo_data_files <- function(demo_dir) {

  created_files <- character()

  # Create student_scores.csv
  scores_file <- file.path(demo_dir, "student_scores.csv")
  scores_data <- data.frame(
    student = c("Alice", "Bob", "Charlie"),
    score = c(85, 92, 78),
    stringsAsFactors = FALSE
  )
  write.csv(scores_data, scores_file, row.names = FALSE)
  created_files <- c(created_files, scores_file)

  # Create grades.csv
  grades_file <- file.path(demo_dir, "grades.csv")
  grades_data <- data.frame(
    subject = c("Math", "Science", "English", "History"),
    avg_grade = c(82.5, 88.3, 79.2, 85.7),
    n_students = c(25, 22, 28, 20),
    stringsAsFactors = FALSE
  )
  write.csv(grades_data, grades_file, row.names = FALSE)
  created_files <- c(created_files, grades_file)

  return(created_files)
}
