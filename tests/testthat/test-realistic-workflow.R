test_that("realistic grading workflow: data in parent, submissions in subdirs", {
  skip_if_not_installed("here")

  # Create a realistic grading folder structure
  tmp_grading <- tempfile()
  dir.create(tmp_grading)
  on.exit(unlink(tmp_grading, recursive = TRUE))

  # Create student submission folders
  student1 <- file.path(tmp_grading, "Student_A_12345")
  student2 <- file.path(tmp_grading, "Student_B_67890")
  dir.create(student1)
  dir.create(student2)

  # Create data files at the parent level (grading folder root)
  data_csv <- file.path(tmp_grading, "test_data.csv")
  writeLines(c("id,score", "1,85", "2,92", "3,78"), data_csv)

  # Create student 1's Rmd with bare file path
  rmd1 <- file.path(student1, "homework.Rmd")
  rmd1_content <- c(
    "---",
    "title: Student A",
    "---",
    "",
    "```{r}",
    'data <- read.csv("test_data.csv")',
    "mean(data$score)",
    "```"
  )
  writeLines(rmd1_content, rmd1)

  # Create student 2's Rmd with bare file path
  rmd2 <- file.path(student2, "homework.Rmd")
  rmd2_content <- c(
    "---",
    "title: Student B",
    "---",
    "",
    "```{r}",
    'data <- read.csv("test_data.csv")',
    "summary(data$score)",
    "```"
  )
  writeLines(rmd2_content, rmd2)

  # Create .here file at grading folder root
  here_file <- file.path(tmp_grading, ".here")
  writeLines("here root", here_file)

  # Change working directory to grading folder (mimics instructor workflow)
  old_wd <- getwd()
  setwd(tmp_grading)
  on.exit(setwd(old_wd), add = TRUE)

  # Process all submissions with data_folder = "."
  result <- fix_folder(
    path = ".",
    recursive = TRUE,
    fix_paths = TRUE,
    data_folder = ".",  # Data is at the parent level
    quiet = TRUE
  )

  # Verify both files were processed
  expect_true(file.exists(file.path(student1, "homework_FIXED.Rmd")))
  expect_true(file.exists(file.path(student2, "homework_FIXED.Rmd")))

  # Verify backups were created
  expect_true(file.exists(paste0(rmd1, ".bak")))
  expect_true(file.exists(paste0(rmd2, ".bak")))

  # Read fixed files and verify paths were converted
  fixed1 <- readLines(file.path(student1, "homework_FIXED.Rmd"))
  fixed2 <- readLines(file.path(student2, "homework_FIXED.Rmd"))

  expect_true(any(grepl('here::here\\("\\.", "test_data.csv"\\)', fixed1)))
  expect_true(any(grepl('here::here\\("\\.", "test_data.csv"\\)', fixed2)))

  # Verify original bare paths are gone
  expect_false(any(grepl('read\\.csv\\("test_data\\.csv"\\)', fixed1)))
  expect_false(any(grepl('read\\.csv\\("test_data\\.csv"\\)', fixed2)))
})

test_that("realistic workflow: chunks execute successfully with parent-level data", {
  skip_if_not_installed("here")

  # Create grading folder structure
  tmp_grading <- tempfile()
  dir.create(tmp_grading)
  on.exit(unlink(tmp_grading, recursive = TRUE))

  # Create student folder
  student_dir <- file.path(tmp_grading, "Student_Test")
  dir.create(student_dir)

  # Create data file at parent level
  data_csv <- file.path(tmp_grading, "grades.csv")
  writeLines(c("student,grade", "Alice,88", "Bob,92"), data_csv)

  # Create student Rmd that will work after fixing
  rmd <- file.path(student_dir, "analysis.Rmd")
  rmd_content <- c(
    "---",
    "title: Analysis",
    "---",
    "",
    "```{r}",
    'grades <- read.csv("grades.csv")',
    "avg <- mean(grades$grade)",
    "```"
  )
  writeLines(rmd_content, rmd)

  # Create .here file
  writeLines("here root", file.path(tmp_grading, ".here"))

  # Change to grading folder
  old_wd <- getwd()
  setwd(tmp_grading)
  on.exit(setwd(old_wd), add = TRUE)

  # Fix the file
  output <- fix_rmd(
    file.path("Student_Test", "analysis.Rmd"),
    fix_paths = TRUE,
    data_folder = ".",
    quiet = TRUE
  )

  # Read the fixed file
  fixed <- readLines(output)

  # Verify path was fixed
  expect_true(any(grepl('here::here\\("\\.", "grades.csv"\\)', fixed)))

  # Verify no eval=FALSE was added (chunk should execute successfully)
  chunk_lines <- fixed[grepl("```\\{r", fixed)]
  expect_false(any(grepl("eval.*FALSE", chunk_lines)))
})

test_that("realistic workflow: handles missing data files gracefully", {
  skip_if_not_installed("here")

  # Create grading folder
  tmp_grading <- tempfile()
  dir.create(tmp_grading)
  on.exit(unlink(tmp_grading, recursive = TRUE))

  # Create student folder
  student_dir <- file.path(tmp_grading, "Student_Missing_Data")
  dir.create(student_dir)

  # Create Rmd that references non-existent data
  rmd <- file.path(student_dir, "homework.Rmd")
  rmd_content <- c(
    "---",
    "title: Test",
    "---",
    "",
    "```{r}",
    'data <- read.csv("missing_file.csv")',
    "```"
  )
  writeLines(rmd_content, rmd)

  # Create .here file
  writeLines("here root", file.path(tmp_grading, ".here"))

  # Change to grading folder
  old_wd <- getwd()
  setwd(tmp_grading)
  on.exit(setwd(old_wd), add = TRUE)

  # Fix the file (should handle error gracefully)
  output <- fix_rmd(
    file.path("Student_Missing_Data", "homework.Rmd"),
    fix_paths = TRUE,
    data_folder = ".",
    quiet = TRUE
  )

  # Read fixed file
  fixed <- readLines(output)

  # Verify path was fixed
  expect_true(any(grepl('here::here\\("\\.", "missing_file.csv"\\)', fixed)))

  # Verify eval=FALSE was added due to missing file
  chunk_header <- fixed[grepl("```\\{r", fixed)]
  expect_true(any(grepl("eval.*FALSE", chunk_header)))
})

test_that("realistic workflow: multiple data files in parent folder", {
  skip_if_not_installed("here")

  # Create grading folder
  tmp_grading <- tempfile()
  dir.create(tmp_grading)
  on.exit(unlink(tmp_grading, recursive = TRUE))

  # Create student folder
  student_dir <- file.path(tmp_grading, "Student_Multi_Data")
  dir.create(student_dir)

  # Create multiple data files at parent level
  writeLines(c("id,x", "1,10", "2,20"), file.path(tmp_grading, "data1.csv"))
  writeLines(c("id,y", "1,30", "2,40"), file.path(tmp_grading, "data2.csv"))

  # Create Rmd using multiple data files
  rmd <- file.path(student_dir, "analysis.Rmd")
  rmd_content <- c(
    "---",
    "title: Multi-file Analysis",
    "---",
    "",
    "```{r}",
    'df1 <- read.csv("data1.csv")',
    'df2 <- read.csv("data2.csv")',
    "combined <- merge(df1, df2, by = 'id')",
    "```"
  )
  writeLines(rmd_content, rmd)

  # Create .here file
  writeLines("here root", file.path(tmp_grading, ".here"))

  # Change to grading folder
  old_wd <- getwd()
  setwd(tmp_grading)
  on.exit(setwd(old_wd), add = TRUE)

  # Fix the file
  output <- fix_rmd(
    file.path("Student_Multi_Data", "analysis.Rmd"),
    fix_paths = TRUE,
    data_folder = ".",
    quiet = TRUE
  )

  # Read fixed file
  fixed <- readLines(output)

  # Verify both paths were fixed
  expect_true(any(grepl('here::here\\("\\.", "data1.csv"\\)', fixed)))
  expect_true(any(grepl('here::here\\("\\.", "data2.csv"\\)', fixed)))

  # Verify chunks executed successfully (no eval=FALSE)
  chunk_lines <- fixed[grepl("```\\{r", fixed)]
  expect_false(any(grepl("eval.*FALSE", chunk_lines)))
})

test_that("realistic workflow: nested student folders", {
  skip_if_not_installed("here")

  # Create grading folder with nested structure
  tmp_grading <- tempfile()
  dir.create(tmp_grading)
  on.exit(unlink(tmp_grading, recursive = TRUE))

  # Create nested folders (e.g., by section)
  section_a <- file.path(tmp_grading, "Section_A")
  section_b <- file.path(tmp_grading, "Section_B")
  dir.create(section_a)
  dir.create(section_b)

  student1 <- file.path(section_a, "Student_1")
  student2 <- file.path(section_b, "Student_2")
  dir.create(student1)
  dir.create(student2)

  # Create data at root
  writeLines(c("x", "1", "2"), file.path(tmp_grading, "data.csv"))

  # Create Rmds in nested locations
  for (student_dir in c(student1, student2)) {
    rmd <- file.path(student_dir, "hw.Rmd")
    writeLines(c(
      "---",
      "title: HW",
      "---",
      "",
      "```{r}",
      'd <- read.csv("data.csv")',
      "```"
    ), rmd)
  }

  # Create .here file
  writeLines("here root", file.path(tmp_grading, ".here"))

  # Change to grading folder
  old_wd <- getwd()
  setwd(tmp_grading)
  on.exit(setwd(old_wd), add = TRUE)

  # Process all files recursively
  fix_folder(".", recursive = TRUE, fix_paths = TRUE, data_folder = ".", quiet = TRUE)

  # Verify both nested files were processed
  expect_true(file.exists(file.path(student1, "hw_FIXED.Rmd")))
  expect_true(file.exists(file.path(student2, "hw_FIXED.Rmd")))
})

test_that("realistic workflow: custom data folder name", {
  skip_if_not_installed("here")

  # Some instructors might put data in a subfolder
  tmp_grading <- tempfile()
  dir.create(tmp_grading)
  on.exit(unlink(tmp_grading, recursive = TRUE))

  # Create data subfolder
  data_dir <- file.path(tmp_grading, "assignment_data")
  dir.create(data_dir)

  # Create student folder
  student_dir <- file.path(tmp_grading, "Student_Custom")
  dir.create(student_dir)

  # Put data in the data subfolder
  writeLines(c("val", "5", "10"), file.path(data_dir, "values.csv"))

  # Create student Rmd with bare path
  rmd <- file.path(student_dir, "work.Rmd")
  writeLines(c(
    "---",
    "title: Work",
    "---",
    "",
    "```{r}",
    'v <- read.csv("values.csv")',
    "```"
  ), rmd)

  # Create .here file
  writeLines("here root", file.path(tmp_grading, ".here"))

  # Change to grading folder
  old_wd <- getwd()
  setwd(tmp_grading)
  on.exit(setwd(old_wd), add = TRUE)

  # Fix with custom data folder
  output <- fix_rmd(
    file.path("Student_Custom", "work.Rmd"),
    fix_paths = TRUE,
    data_folder = "assignment_data",
    quiet = TRUE
  )

  # Read fixed file
  fixed <- readLines(output)

  # Verify path points to custom folder
  expect_true(any(grepl('here::here\\("assignment_data", "values.csv"\\)', fixed)))
})
