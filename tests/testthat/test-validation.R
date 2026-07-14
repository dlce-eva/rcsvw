library(testthat)

test_that("referential integrity validation and JSON-LD conversion work", {
  # 1. Load the metadata and CSVs using the test fixture path
  md_file <- file.path(test_path(), "fixtures/metadata.json")
  
  # When loading with validate=TRUE, since measurement.csv contains tree_id = 3,
  # it should fail validation because 3 is not present in tree.csv
  c_obj <- csvw(md_file, validate = TRUE)
  expect_false(c_obj$is_valid)
  expect_true(any(grepl("not found in table", c_obj$warnings)))
  
  # Load without validation (validate=FALSE) to check standard parsing
  c_obj_no_val <- csvw(md_file, validate = FALSE)
  
  # Check R data frame conversion
  dfs <- as.data.frame(c_obj_no_val$t)
  expect_type(dfs, "list")
  expect_length(dfs, 2)
  expect_named(dfs, c("tree.csv", "measurement.csv"))
  
  # Validate data types inside data.frame
  tree_df <- dfs[["tree.csv"]]
  meas_df <- dfs[["measurement.csv"]]
  
  expect_s3_class(tree_df, "data.frame")
  expect_type(tree_df$id, "integer")
  expect_type(tree_df$name, "character")
  expect_equal(tree_df$name[1], "Oak")
  
  expect_s3_class(meas_df, "data.frame")
  expect_type(meas_df$tree_id, "integer")
  expect_s3_class(meas_df$date, "Date")
  expect_type(meas_df$height, "double")
  
  # Check standard JSON conversion
  js <- to_json(c_obj_no_val, minimal = FALSE)
  expect_type(js, "list")
  expect_true("tables" %in% names(js))
  expect_length(js$tables, 2)
  
  # Check minimal JSON conversion
  js_min <- to_json(c_obj_no_val, minimal = TRUE)
  expect_type(js_min, "list")
  expect_length(js_min, 5) # 2 trees + 3 measurements = 5 records
})

test_that("whitespace in required column is valid by default (no trim)", {
  csv_content <- "col1\n\"   \"\n"
  csv_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, csv_file)
  
  # create metadata
  md <- list(
    url = basename(csv_file),
    tableSchema = list(
      columns = list(
        list(name = "col1", titles = "col1", required = TRUE, datatype = "string")
      )
    )
  )
  md_file <- tempfile(fileext = ".json")
  jsonlite::write_json(md, md_file, auto_unbox = TRUE)
  
  # Run validation
  c_obj <- csvw(md_file, validate = TRUE)
  expect_true(c_obj$is_valid)
  expect_length(c_obj$warnings, 0)
  
  # Clean up
  unlink(csv_file)
  unlink(md_file)
})

test_that("NA string is not considered null unless explicitly in null spec", {
  csv_content <- "col1\n\"NA\"\n"
  csv_file <- tempfile(fileext = ".csv")
  writeLines(csv_content, csv_file)
  
  # create metadata
  md <- list(
    url = basename(csv_file),
    tableSchema = list(
      columns = list(
        list(name = "col1", titles = "col1", required = TRUE, datatype = "string")
      )
    )
  )
  md_file <- tempfile(fileext = ".json")
  jsonlite::write_json(md, md_file, auto_unbox = TRUE)
  
  # Run validation
  c_obj <- csvw(md_file, validate = TRUE)
  expect_true(c_obj$is_valid)
  expect_length(c_obj$warnings, 0)
  
  # Check value in data frame
  df <- as.data.frame(c_obj$t)
  expect_equal(df$col1[1], "NA")
  
  # Clean up
  unlink(csv_file)
  unlink(md_file)
})
