test_that("fix_and_knit_folder() requires valid directory", {
  expect_error(
    fix_and_knit_folder(path = "nonexistent_folder"),
    "Directory does not exist"
  )
})

test_that("fix_and_knit_folder() works with single file", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a simple Rmd file (not yet fixed)
  rmd_content <- c(
    "---",
    "title: Test Document",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "x <- 1 + 1",
    "print(x)",
    "```"
  )

  rmd_file <- file.path(tmp_dir, "test.Rmd")
  writeLines(rmd_content, rmd_file)

  # Run combined workflow
  results <- fix_and_knit_folder(path = tmp_dir, quiet = TRUE, knit_quiet = TRUE)

  # Check structure
  expect_type(results, "list")
  expect_equal(names(results), c("fix_results", "knit_results"))
  expect_type(results$fix_results, "character")  # fix_folder returns char vector
  expect_true(is.data.frame(results$knit_results))

  # Check fix results (should have one file)
  expect_equal(length(results$fix_results), 1)
  expect_match(results$fix_results[1], "_FIXED\\.Rmd$")

  # Check knit results
  expect_equal(nrow(results$knit_results), 1)
  expect_true(results$knit_results$success[1])
  expect_false(is.na(results$knit_results$output[1]))
  expect_true(file.exists(results$knit_results$output[1]))
})

test_that("fix_and_knit_folder() handles multiple files", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create two simple Rmd files
  rmd_content <- c(
    "---",
    "title: Test",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "x <- 1 + 1",
    "```"
  )

  file1 <- file.path(tmp_dir, "file1.Rmd")
  file2 <- file.path(tmp_dir, "file2.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Run combined workflow
  results <- fix_and_knit_folder(path = tmp_dir, quiet = TRUE, knit_quiet = TRUE)

  # Check both were processed
  expect_equal(length(results$fix_results), 2)
  expect_equal(nrow(results$knit_results), 2)
  expect_true(all(results$knit_results$success))
})

test_that("fix_and_knit_folder() works recursively", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  subdir <- file.path(tmp_dir, "subdir")
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

  # Create files in both directories
  file1 <- file.path(tmp_dir, "root.Rmd")
  file2 <- file.path(subdir, "sub.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Test recursive = TRUE
  results_recursive <- fix_and_knit_folder(
    path = tmp_dir,
    recursive = TRUE,
    quiet = TRUE,
    knit_quiet = TRUE
  )
  expect_equal(nrow(results_recursive$knit_results), 2)

  # Clean up and test recursive = FALSE
  unlink(results_recursive$knit_results$output)

  results_non_recursive <- fix_and_knit_folder(
    path = tmp_dir,
    recursive = FALSE,
    quiet = TRUE,
    knit_quiet = TRUE
  )
  expect_equal(nrow(results_non_recursive$knit_results), 1)
})

test_that("fix_and_knit_folder() uses output_dir for collected outputs", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  student1 <- file.path(tmp_dir, "Student1")
  student2 <- file.path(tmp_dir, "Student2")
  dir.create(student1)
  dir.create(student2)
  output_dir <- file.path(tmp_dir, "outputs")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Assignment",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```"
  )

  # Both students have same filename
  file1 <- file.path(student1, "assignment.Rmd")
  file2 <- file.path(student2, "assignment.Rmd")
  writeLines(rmd_content, file1)
  writeLines(rmd_content, file2)

  # Run with output_dir
  results <- fix_and_knit_folder(
    path = tmp_dir,
    output_dir = output_dir,
    quiet = TRUE,
    knit_quiet = TRUE
  )

  # Check outputs are in output_dir
  expect_true(dir.exists(output_dir))
  expect_true(all(results$knit_results$success))
  expect_true(all(grepl(output_dir, results$knit_results$output)))

  # Check unique names (should include student folder names)
  outputs <- basename(results$knit_results$output)
  expect_equal(length(unique(outputs)), 2)
  expect_true(any(grepl("Student1", outputs)))
  expect_true(any(grepl("Student2", outputs)))
})

test_that("fix_and_knit_folder() handles mixed success and failure", {
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

  # Bad file (invalid YAML will fail knitting)
  bad_rmd <- c(
    "---",
    "title: Bad",
    "output: invalid_format_that_doesnt_exist",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```"
  )

  good_file <- file.path(tmp_dir, "good.Rmd")
  bad_file <- file.path(tmp_dir, "bad.Rmd")
  writeLines(good_rmd, good_file)
  writeLines(bad_rmd, bad_file)

  # Run combined workflow
  results <- fix_and_knit_folder(path = tmp_dir, quiet = TRUE, knit_quiet = TRUE)

  # Check results
  expect_equal(nrow(results$knit_results), 2)
  expect_equal(sum(results$knit_results$success), 1)
  expect_equal(sum(!results$knit_results$success), 1)

  # Good file should succeed
  good_result <- results$knit_results[grepl("good", results$knit_results$file), ]
  expect_true(good_result$success)

  # Bad file should fail
  bad_result <- results$knit_results[grepl("bad", results$knit_results$file), ]
  expect_false(bad_result$success)
  expect_false(is.na(bad_result$error))
})

test_that("fix_and_knit_folder() respects quiet parameter", {
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

  rmd_file <- file.path(tmp_dir, "test.Rmd")
  writeLines(rmd_content, rmd_file)

  # Capture output with quiet = TRUE
  output_quiet <- capture.output({
    results_quiet <- fix_and_knit_folder(
      path = tmp_dir,
      quiet = TRUE,
      knit_quiet = TRUE
    )
  })

  # Clean up
  unlink(results_quiet$knit_results$output)

  # Capture output with quiet = FALSE
  output_verbose <- capture.output({
    results_verbose <- fix_and_knit_folder(
      path = tmp_dir,
      quiet = FALSE,
      knit_quiet = FALSE
    )
  })

  # Quiet mode should produce less output
  expect_true(length(output_quiet) < length(output_verbose))

  # Both should process the file
  expect_equal(nrow(results_quiet$knit_results), 1)
  expect_equal(nrow(results_verbose$knit_results), 1)
})

test_that("fix_and_knit_folder() returns correct structure", {
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

  rmd_file <- file.path(tmp_dir, "test.Rmd")
  writeLines(rmd_content, rmd_file)

  results <- fix_and_knit_folder(path = tmp_dir, quiet = TRUE, knit_quiet = TRUE)

  # Check structure
  expect_type(results, "list")
  expect_equal(names(results), c("fix_results", "knit_results"))

  # Check fix_results structure (should be char vector from fix_folder)
  expect_type(results$fix_results, "character")
  expect_true(length(results$fix_results) > 0)

  # Check knit_results structure
  expect_true(is.data.frame(results$knit_results))
  expect_equal(names(results$knit_results), c("file", "output", "success", "error"))
})

test_that("fix_and_knit_folder() processes with add_student_info", {
  skip_if_not_installed("rmarkdown")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  student_dir <- file.path(tmp_dir, "StudentA_12345")
  dir.create(student_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  rmd_content <- c(
    "---",
    "title: Homework",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "x <- 1",
    "```"
  )

  rmd_file <- file.path(student_dir, "assignment.Rmd")
  writeLines(rmd_content, rmd_file)

  # Run with add_student_info
  results <- fix_and_knit_folder(
    path = tmp_dir,
    add_student_info = TRUE,
    quiet = TRUE,
    knit_quiet = TRUE
  )

  # Should process successfully
  expect_equal(length(results$fix_results), 1)
  expect_equal(nrow(results$knit_results), 1)
  expect_true(results$knit_results$success[1])

  # Check that fixed file contains student info
  fixed_file <- file.path(student_dir, "assignment_FIXED.Rmd")
  expect_true(file.exists(fixed_file))
  fixed_content <- readLines(fixed_file)
  # Should contain student folder name as heading
  expect_true(any(grepl("StudentA_12345", fixed_content)))
})
