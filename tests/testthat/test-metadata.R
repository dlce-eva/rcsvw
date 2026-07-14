library(testthat)

test_that("metadata S3 classes parsing and construction works", {
  # Column parsing
  col_list <- list(name = "age", datatype = "integer", required = TRUE)
  col <- parse_column(col_list)
  expect_s3_class(col, "csvw_column")
  expect_equal(col$name, "age")
  expect_true(col$required)
  
  # Schema parsing
  schema_list <- list(
    columns = list(
      list(name = "id", datatype = "string"),
      list(name = "val", datatype = "decimal")
    ),
    primaryKey = "id"
  )
  schema <- parse_table_schema(schema_list)
  expect_s3_class(schema, "csvw_table_schema")
  expect_length(schema$columns, 2)
  expect_equal(schema$primaryKey, "id")
  
  # Table parsing
  table_list <- list(
    url = "data.csv",
    tableSchema = schema_list
  )
  tbl <- parse_table(table_list, base_path = "/tmp")
  expect_s3_class(tbl, "csvw_table")
  expect_equal(basename(tbl$url), "data.csv")
})

test_that("property inheritance works", {
  # Null property inheritance from Table to Column
  table_list <- list(
    url = "data.csv",
    null = "N/A",
    tableSchema = list(
      columns = list(
        list(name = "col1"),
        list(name = "col2", null = "NULL")
      )
    )
  )
  tbl <- parse_table(table_list)
  col1 <- tbl$tableSchema$columns[[1]]
  col2 <- tbl$tableSchema$columns[[2]]
  
  expect_equal(inherit_null(col1), "N/A")
  expect_equal(inherit_null(col2), "NULL")
})

test_that("reading zipped CSV tables works when .zip is appended to the URL", {
  csv_file <- tempfile(fileext = ".csv")
  writeLines("ID,Name\n1,Alice\n2,Bob", csv_file)
  on.exit(unlink(csv_file), add = TRUE)
  
  zip_file <- paste0(csv_file, ".zip")
  on.exit(unlink(zip_file), add = TRUE)
  
  zip_res <- tryCatch({
    zip(zip_file, files = csv_file, flags = "-q -j")
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!zip_res || !file.exists(zip_file)) {
    skip("zip utility not available")
  }
  
  unlink(csv_file)
  expect_false(file.exists(csv_file))
  expect_true(file.exists(zip_file))
  
  tbl <- parse_table(list(
    url = csv_file,
    tableSchema = list(
      columns = list(
        list(name = "ID", datatype = "integer"),
        list(name = "Name", datatype = "string")
      )
    )
  ))
  
  rows <- read_table_csv(tbl, strict = TRUE)
  expect_length(rows, 2)
  expect_equal(rows[[1]]$ID, 1)
  expect_equal(rows[[1]]$Name, "Alice")
  expect_equal(rows[[2]]$ID, 2)
  expect_equal(rows[[2]]$Name, "Bob")
})

test_that("list-valued foreign keys are validated correctly", {
  users_file <- tempfile(fileext = ".csv")
  writeLines("ID,Roles\n1,admin user\n2,user guest\n3,superadmin", users_file)
  on.exit(unlink(users_file), add = TRUE)
  
  roles_file <- tempfile(fileext = ".csv")
  writeLines("Name\nadmin\nuser\nguest", roles_file)
  on.exit(unlink(roles_file), add = TRUE)
  
  tg_meta <- list(
    tables = list(
      list(
        url = users_file,
        tableSchema = list(
          columns = list(
            list(name = "ID", datatype = "integer"),
            list(name = "Roles", separator = " ")
          ),
          foreignKeys = list(
            list(
              columnReference = "Roles",
              reference = list(
                resource = basename(roles_file),
                columnReference = "Name"
              )
            )
          )
        )
      ),
      list(
        url = roles_file,
        tableSchema = list(
          columns = list(
            list(name = "Name", datatype = "string")
          ),
          primaryKey = "Name"
        )
      )
    )
  )
  
  g_fail <- parse_table_group(tg_meta)
  g_fail$tables[[1]]$data <- read_table_csv(g_fail$tables[[1]], strict = FALSE)
  g_fail$tables[[2]]$data <- read_table_csv(g_fail$tables[[2]], strict = FALSE)
  
  val_res <- rcsvw:::check_referential_integrity(g_fail)
  expect_false(val_res)
  
  writeLines("ID,Roles\n1,admin user\n2,user guest", users_file)
  g_pass <- parse_table_group(tg_meta)
  g_pass$tables[[1]]$data <- read_table_csv(g_pass$tables[[1]], strict = FALSE)
  g_pass$tables[[2]]$data <- read_table_csv(g_pass$tables[[2]], strict = FALSE)
  
  val_res_pass <- rcsvw:::check_referential_integrity(g_pass)
  expect_true(val_res_pass)
})

test_that("lax validation allows null values in foreign key columns", {
  users_file <- tempfile(fileext = ".csv")
  writeLines("ID,Roles\n1,admin\n2,\n3,user", users_file)
  on.exit(unlink(users_file), add = TRUE)
  
  roles_file <- tempfile(fileext = ".csv")
  writeLines("Name\nadmin\nuser", roles_file)
  on.exit(unlink(roles_file), add = TRUE)
  
  tg_meta <- list(
    tables = list(
      list(
        url = users_file,
        tableSchema = list(
          columns = list(
            list(name = "ID", datatype = "integer"),
            list(name = "Roles", null = "")
          ),
          foreignKeys = list(
            list(
              columnReference = "Roles",
              reference = list(
                resource = basename(roles_file),
                columnReference = "Name"
              )
            )
          )
        )
      ),
      list(
        url = roles_file,
        tableSchema = list(
          columns = list(
            list(name = "Name", datatype = "string")
          ),
          primaryKey = "Name"
        )
      )
    )
  )
  
  g_val_fail <- parse_table_group(tg_meta)
  g_val_fail$tables[[1]]$data <- read_table_csv(g_val_fail$tables[[1]], strict = FALSE)
  g_val_fail$tables[[2]]$data <- read_table_csv(g_val_fail$tables[[2]], strict = FALSE)
  
  val_res <- rcsvw:::check_referential_integrity(g_val_fail, strict = TRUE)
  expect_false(val_res)
  
  val_res_lax <- rcsvw:::check_referential_integrity(g_val_fail, strict = FALSE)
  expect_true(val_res_lax)
})

test_that("compatibility matching checks name first and then titles", {
  csv_file <- tempfile(fileext = ".csv")
  writeLines("col_name\nvalue1", csv_file)
  on.exit(unlink(csv_file), add = TRUE)
  
  tbl <- parse_table(list(
    url = csv_file,
    tableSchema = list(
      columns = list(
        list(name = "col_name", titles = "col_title", datatype = "string")
      )
    )
  ))
  
  rows <- read_table_csv(tbl, strict = TRUE)
  expect_length(rows, 1)
  expect_equal(rows[[1]]$col_name, "value1")
})
