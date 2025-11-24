test_that("knit_fixed_files() requires valid directory", {
  expect_error(
    knit_fixed_files(path = "nonexistent_folder"),
    "Directory does not exist"
  )
})

test_that("knit_fixed_files() handles empty directory gracefully", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  result <- knit_fixed_files(path = tmp_dir, quiet = TRUE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("file", "output", "success", "error"))
})

test_that("knit_fixed_files() finds and knits fixed files", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a simple fixed Rmd file
  rmd_content <- c(
    "---",
    "title: Test Document",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "x <- 1 + 1",
    "```"
  )

  rmd_file <- file.path(tmp_dir, "test_FIXED.Rmd")
  writeLines(rmd_content, rmd_file)

  # Knit the file
  result <- knit_fixed_files(path = tmp_dir, quiet = TRUE)

  # Check results
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$file[1], rmd_file)
  expect_true(result$success[1])
  expect_false(is.na(result$output[1]))
  expect_true(is.na(result$error[1]))

  # Check output file exists
  expect_true(file.exists(result$output[1]))
  expect_match(result$output[1], "\\.html$")
})

test_that("knit_fixed_files() handles multiple files", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create multiple fixed Rmd files
  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "Test content"
  )

  file1 <- file.path(tmp_dir, "student1_FIXED.Rmd")
  file2 <- file.path(tmp_dir, "student2_FIXED.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Knit all files
  result <- knit_fixed_files(path = tmp_dir, quiet = TRUE)

  # Check results
  expect_equal(nrow(result), 2)
  expect_true(all(result$success))
  expect_true(all(!is.na(result$output)))
  expect_true(all(file.exists(result$output)))
})

test_that("knit_fixed_files() works recursively", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  subdir <- file.path(tmp_dir, "subfolder")
  dir.create(subdir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "Test"
  )

  # Create files in both levels
  file1 <- file.path(tmp_dir, "top_FIXED.Rmd")
  file2 <- file.path(subdir, "sub_FIXED.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Test recursive = TRUE
  result_recursive <- knit_fixed_files(path = tmp_dir, recursive = TRUE, quiet = TRUE)
  expect_equal(nrow(result_recursive), 2)

  # Clean up output files
  unlink(result_recursive$output)

  # Test recursive = FALSE
  result_non_recursive <- knit_fixed_files(path = tmp_dir, recursive = FALSE, quiet = TRUE)
  expect_equal(nrow(result_non_recursive), 1)
  expect_match(result_non_recursive$file[1], "top_FIXED\\.Rmd$")
})

test_that("knit_fixed_files() respects custom pattern", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "Test"
  )

  # Create files with different naming
  file1 <- file.path(tmp_dir, "report_FIXED.Rmd")
  file2 <- file.path(tmp_dir, "analysis_REPAIRED.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Default pattern should only find _FIXED
  result_default <- knit_fixed_files(path = tmp_dir, quiet = TRUE)
  expect_equal(nrow(result_default), 1)
  expect_match(result_default$file[1], "_FIXED\\.Rmd$")

  # Clean up
  unlink(result_default$output)

  # Custom pattern should find _REPAIRED
  result_custom <- knit_fixed_files(
    path = tmp_dir,
    pattern = "_REPAIRED\\.Rmd$",
    quiet = TRUE
  )
  expect_equal(nrow(result_custom), 1)
  expect_match(result_custom$file[1], "_REPAIRED\\.Rmd$")
})

test_that("knit_fixed_files() creates output_dir if needed", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  output_dir <- file.path(tmp_dir, "outputs", "nested")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "Test"
  )

  rmd_file <- file.path(tmp_dir, "test_FIXED.Rmd")
  writeLines(rmd_content, rmd_file)

  # output_dir doesn't exist yet
  expect_false(dir.exists(output_dir))

  # Knit with output_dir
  result <- knit_fixed_files(path = tmp_dir, output_dir = output_dir, quiet = TRUE)

  # Check output_dir was created
  expect_true(dir.exists(output_dir))
  expect_true(result$success[1])
  expect_true(file.exists(result$output[1]))

  # Check output is in output_dir
  expect_match(result$output[1], output_dir)
})

test_that("knit_fixed_files() uses unique output names with output_dir", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  student1 <- file.path(tmp_dir, "StudentA")
  student2 <- file.path(tmp_dir, "StudentB")
  dir.create(student1)
  dir.create(student2)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Assignment",
    "output: html_document",
    "---",
    "",
    "Work"
  )

  # Both students have same filename
  file1 <- file.path(student1, "assignment_FIXED.Rmd")
  file2 <- file.path(student2, "assignment_FIXED.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Knit with output_dir
  result <- knit_fixed_files(path = tmp_dir, output_dir = output_dir, quiet = TRUE)

  expect_equal(nrow(result), 2)
  expect_true(all(result$success))

  # Outputs should have unique names (include parent folder)
  outputs <- basename(result$output)
  expect_true(any(grepl("StudentA", outputs)))
  expect_true(any(grepl("StudentB", outputs)))
  expect_equal(length(unique(outputs)), 2)
})

test_that("knit_fixed_files() handles knitting errors gracefully", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create Rmd with intentional error
  bad_rmd <- c(
    "---",
    "title: Bad Document",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "stop('Intentional error')",
    "```"
  )

  bad_file <- file.path(tmp_dir, "bad_FIXED.Rmd")
  writeLines(bad_rmd, bad_file)

  # Knit should capture error
  result <- knit_fixed_files(path = tmp_dir, quiet = TRUE)

  expect_equal(nrow(result), 1)
  expect_false(result$success[1])
  expect_true(is.na(result$output[1]))
  expect_false(is.na(result$error[1]))
  expect_true(nchar(result$error[1]) > 0)
})

test_that("knit_fixed_files() handles mixed success and failure", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Good file
  good_rmd <- c(
    "---",
    "title: Good",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```"
  )

  # Bad file
  bad_rmd <- c(
    "---",
    "title: Bad",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "stop('Error!')",
    "```"
  )

  good_file <- file.path(tmp_dir, "good_FIXED.Rmd")
  bad_file <- file.path(tmp_dir, "bad_FIXED.Rmd")
  writeLines(good_rmd, good_file)
  writeLines(bad_rmd, bad_file)

  # Knit all
  result <- knit_fixed_files(path = tmp_dir, quiet = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(sum(result$success), 1)
  expect_equal(sum(!result$success), 1)

  # Good file should have output
  good_result <- result[grepl("good_FIXED", result$file), ]
  expect_true(good_result$success)
  expect_false(is.na(good_result$output))

  # Bad file should have error
  bad_result <- result[grepl("bad_FIXED", result$file), ]
  expect_false(bad_result$success)
  expect_true(is.na(bad_result$output))
  expect_false(is.na(bad_result$error))
})

test_that("knit_fixed_files() respects quiet parameter", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "Test"
  )

  rmd_file <- file.path(tmp_dir, "test_FIXED.Rmd")
  writeLines(rmd_content, rmd_file)

  # Capture output for quiet = TRUE
  output_quiet <- capture.output({
    result_quiet <- knit_fixed_files(path = tmp_dir, quiet = TRUE)
  })

  # Clean up for next test
  unlink(result_quiet$output)

  # Capture output for quiet = FALSE
  output_verbose <- capture.output({
    result_verbose <- knit_fixed_files(path = tmp_dir, quiet = FALSE)
  })

  # Quiet mode should produce less output
  expect_true(length(output_quiet) < length(output_verbose))

  # Both should succeed
  expect_true(result_quiet$success[1])
  expect_true(result_verbose$success[1])
})

test_that("knit_fixed_files() returns correct data frame structure", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "Test"
  )

  rmd_file <- file.path(tmp_dir, "test_FIXED.Rmd")
  writeLines(rmd_content, rmd_file)

  result <- knit_fixed_files(path = tmp_dir, quiet = TRUE)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("file", "output", "success", "error"))
  expect_type(result$file, "character")
  expect_type(result$output, "character")
  expect_type(result$success, "logical")
  expect_type(result$error, "character")
  expect_equal(nrow(result), 1)
})

test_that("knit_fixed_files() has rmarkdown package check", {
  # This test verifies the error message exists in the code
  # We can't easily mock requireNamespace due to locked bindings

  # Read the function source
  func_body <- deparse(knit_fixed_files)
  func_text <- paste(func_body, collapse = "\n")

  # Verify the check exists
  expect_true(grepl("requireNamespace.*rmarkdown", func_text))
  expect_true(grepl("rmarkdown.*required", func_text))
})
