test_that("fix_folder processes multiple files", {
  # Create a temporary directory with multiple Rmd files
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create test files
  file1 <- file.path(tmp_dir, "test1.Rmd")
  file2 <- file.path(tmp_dir, "test2.Rmd")

  writeLines(c("---", "title: Test1", "---", "", "```{r}", "x <- 1", "```"), file1)
  writeLines(c("---", "title: Test2", "---", "", "```{r}", "y <- 2", "```"), file2)

  # Process folder
  result <- fix_folder(tmp_dir, quiet = TRUE)

  # Check that output files exist
  expect_true(file.exists(file.path(tmp_dir, "test1_FIXED.Rmd")))
  expect_true(file.exists(file.path(tmp_dir, "test2_FIXED.Rmd")))

  # Check that backups exist
  expect_true(file.exists(paste0(file1, ".bak")))
  expect_true(file.exists(paste0(file2, ".bak")))
})

test_that("fix_folder handles recursive directories", {
  # Create nested directory structure
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  sub_dir <- file.path(tmp_dir, "student_A")
  dir.create(sub_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create files in both directories
  file1 <- file.path(tmp_dir, "test1.Rmd")
  file2 <- file.path(sub_dir, "test2.Rmd")

  writeLines(c("---", "title: Test1", "---"), file1)
  writeLines(c("---", "title: Test2", "---"), file2)

  # Process recursively
  result <- fix_folder(tmp_dir, recursive = TRUE, quiet = TRUE)

  # Both files should be processed
  expect_true(file.exists(file.path(tmp_dir, "test1_FIXED.Rmd")))
  expect_true(file.exists(file.path(sub_dir, "test2_FIXED.Rmd")))
})

test_that("fix_folder handles non-recursive mode", {
  # Create nested directory structure
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  sub_dir <- file.path(tmp_dir, "student_A")
  dir.create(sub_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create files in both directories
  file1 <- file.path(tmp_dir, "test1.Rmd")
  file2 <- file.path(sub_dir, "test2.Rmd")

  writeLines(c("---", "title: Test1", "---"), file1)
  writeLines(c("---", "title: Test2", "---"), file2)

  # Process non-recursively
  result <- fix_folder(tmp_dir, recursive = FALSE, quiet = TRUE)

  # Only top-level file should be processed
  expect_true(file.exists(file.path(tmp_dir, "test1_FIXED.Rmd")))
  expect_false(file.exists(file.path(sub_dir, "test2_FIXED.Rmd")))
})

test_that("fix_folder validates directory exists", {
  expect_error(
    fix_folder("nonexistent_directory"),
    "Directory does not exist"
  )
})

test_that("fix_folder handles empty directories gracefully", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Should not error, just message
  expect_message(
    fix_folder(tmp_dir),
    "No R Markdown files found"
  )
})

test_that("fix_folder continues after individual file errors", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a valid file
  file1 <- file.path(tmp_dir, "test1.Rmd")
  writeLines(c("---", "title: Test1", "---"), file1)

  # Create a malformed file (not really Rmd format, but has .Rmd extension)
  file2 <- file.path(tmp_dir, "test2.Rmd")
  writeLines(c("```{r}", "x <- 1"), file2)  # Missing closing fence

  # Should process what it can
  result <- fix_folder(tmp_dir, quiet = TRUE)

  # First file should be processed
  expect_true(file.exists(file.path(tmp_dir, "test1_FIXED.Rmd")))
})

test_that("fix_folder respects pattern argument", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create files with different names
  file1 <- file.path(tmp_dir, "homework.Rmd")
  file2 <- file.path(tmp_dir, "test.Rmd")

  writeLines(c("---", "title: Homework", "---"), file1)
  writeLines(c("---", "title: Test", "---"), file2)

  # Only process files matching "homework"
  result <- fix_folder(tmp_dir, pattern = "homework.*\\.Rmd$", quiet = TRUE)

  # Only homework file should be processed
  expect_true(file.exists(file.path(tmp_dir, "homework_FIXED.Rmd")))
  expect_false(file.exists(file.path(tmp_dir, "test_FIXED.Rmd")))
})
