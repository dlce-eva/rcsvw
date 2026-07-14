library(testthat)
library(rcsvw)

test_that("get_table works to access tables by basename of URL", {
  md_file <- test_path("fixtures/metadata.json")
  res <- csvw(md_file)
  
  # Retrieve tree table
  df_tree <- get_table(res, "tree.csv")
  expect_s3_class(df_tree, "data.frame")
  expect_equal(nrow(df_tree), 2)
  expect_equal(df_tree$id, c(1, 2))
  expect_equal(df_tree$name, c("Oak", "Maple"))
  
  # Retrieve measurement table
  df_meas <- get_table(res, "measurement.csv")
  expect_s3_class(df_meas, "data.frame")
  expect_equal(nrow(df_meas), 3)
  expect_equal(df_meas$tree_id, c(1, 2, 3))
  expect_equal(df_meas$height, c(15.2, 10.4, 8.0))
  
  # Retrieve non-existing table returns NULL
  expect_null(get_table(res, "non_existent.csv"))
})

test_that("as.data.frame handles list-columns containing NULL values cleanly", {
  col1 <- parse_column(list(name = "id", datatype = "integer"))
  col2 <- parse_column(list(name = "tags", datatype = "string", separator = ";"))
  
  tbl_schema <- list(
    columns = list(col1, col2),
    primaryKey = "id"
  )
  class(tbl_schema) <- "csvw_table_schema"
  
  table_desc <- list(
    url = "mock.csv",
    tableSchema = tbl_schema,
    data = list(
      list(id = 1, tags = list("A", "B"), "_row" = 1, "_sourceRow" = 2),
      list(id = 2, tags = list(NULL), "_row" = 2, "_sourceRow" = 3)
    )
  )
  class(table_desc) <- "csvw_table"
  
  expect_no_error(df <- as.data.frame(table_desc))
  expect_equal(nrow(df), 2)
  expect_equal(df$tags[[1]], list("A", "B"))
  expect_equal(df$tags[[2]], list(NA))
})

test_that("unmatched metadata columns are ignored during non-validating reading", {
  tmp_md <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_md), add = TRUE)
  
  md_content <- list(
    "@context" = "http://www.w3.org/ns/csvw",
    "url" = "tree.csv",
    "tableSchema" = list(
      "columns" = list(
        list("name" = "id", "datatype" = "integer"),
        list("name" = "mismatched_title", "titles" = "Expected Title")
      )
    )
  )
  jsonlite::write_json(md_content, tmp_md, auto_unbox = TRUE)
  
  tmp_csv <- file.path(dirname(tmp_md), "tree.csv")
  file.copy(test_path("fixtures/tree.csv"), tmp_csv, overwrite = TRUE)
  on.exit(unlink(tmp_csv), add = TRUE)
  
  # Non-validating read: should succeed and ignore the mismatched column
  expect_no_error(
    res_no_val <- csvw(tmp_csv, md_url = tmp_md, validate = FALSE)
  )
  
  df_tree <- as.data.frame(res_no_val$tables[[1]])
  expect_true("name" %in% names(df_tree))
  expect_false("mismatched_title" %in% names(df_tree))
})

test_that("excess metadata columns are ignored during lax validation", {
  # Write a temporary metadata file describing tree.csv but with mismatched/excess column "mismatched_title"
  tmp_md <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_md), add = TRUE)
  
  # actual tree.csv has columns "id" and "name"
  # we describe it as having "id" and "mismatched_title"
  md_content <- list(
    "@context" = "http://www.w3.org/ns/csvw",
    "url" = "tree.csv",
    "tableSchema" = list(
      "columns" = list(
        list("name" = "id", "datatype" = "integer"),
        list("name" = "mismatched_title", "titles" = "Expected Title")
      )
    )
  )
  jsonlite::write_json(md_content, tmp_md, auto_unbox = TRUE)
  
  # Copy tree.csv to temp directory next to tmp_md so relative URL works
  tmp_csv <- file.path(dirname(tmp_md), "tree.csv")
  file.copy(test_path("fixtures/tree.csv"), tmp_csv, overwrite = TRUE)
  on.exit(unlink(tmp_csv), add = TRUE)
  
  # Validating with lax=TRUE: should ignore mismatched/excess columns and succeed
  expect_no_error(
    res_lax <- csvw(tmp_csv, md_url = tmp_md, validate = TRUE, lax = TRUE)
  )
  
  df_tree <- as.data.frame(res_lax$tables[[1]])
  expect_true("name" %in% names(df_tree))
  expect_false("mismatched_title" %in% names(df_tree))
  
  # Validating with lax=FALSE: should fail
  expect_error(
    csvw(tmp_csv, md_url = tmp_md, validate = TRUE, lax = FALSE)
  )
})
