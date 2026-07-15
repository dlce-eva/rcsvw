library(testthat)

test_that("custom CSV dialect reading works", {
  dialect <- csvw_dialect(
    delimiter = ";",
    skipRows = 2
  )
  
  tbl <- parse_table(list(
    url = "fixtures/test_dialect.csv",
    dialect = dialect
  ), base_path = test_path())
  
  rows <- read_table_csv(tbl, strict = TRUE)
  expect_length(rows, 2)
  expect_equal(rows[[1]]$ID, "1")
  expect_equal(rows[[1]]$Name, "Alice")
  expect_equal(rows[[1]]$Value, "10.5")
  expect_equal(rows[[2]]$ID, "2")
  expect_equal(rows[[2]]$Name, "Bob")
  expect_equal(rows[[2]]$Value, "20.3")
})

test_that("tab delimiter specified as literal tab or escaped string works", {
  # Write a temp tsv file
  tmp <- tempfile(fileext = ".tsv")
  writeLines("ID\tName\tValue\n1\tAlice\t10.5\n2\tBob\t20.3", tmp)
  on.exit(unlink(tmp))
  
  # Test with escaped tab sequence "\\t"
  dialect_esc <- csvw_dialect(delimiter = "\\t")
  tbl_esc <- parse_table(list(
    url = tmp,
    dialect = dialect_esc
  ))
  
  rows_esc <- read_table_csv(tbl_esc, strict = TRUE)
  expect_length(rows_esc, 2)
  expect_equal(rows_esc[[1]]$ID, "1")
  expect_equal(rows_esc[[1]]$Name, "Alice")
  
  # Test with literal tab "\t"
  dialect_lit <- csvw_dialect(delimiter = "\t")
  tbl_lit <- parse_table(list(
    url = tmp,
    dialect = dialect_lit
  ))
  
  rows_lit <- read_table_csv(tbl_lit, strict = TRUE)
  expect_length(rows_lit, 2)
  expect_equal(rows_lit[[1]]$ID, "1")
  expect_equal(rows_lit[[1]]$Name, "Alice")
})

test_that("table inherits dialect from table group in metadata structure", {
  # Write a temp tsv file
  tmp <- tempfile(fileext = ".tsv")
  writeLines("ID\tName\tValue\n1\tAlice\t10.5\n2\tBob\t20.3", tmp)
  on.exit(unlink(tmp))
  
  # A metadata-like list where the dialect (tab delimiter) is defined at the TableGroup level,
  # and the table itself has no dialect property.
  tg_meta <- list(
    dialect = list(delimiter = "\\t"),
    tables = list(
      list(url = tmp)
    )
  )
  
  parsed_group <- parse_table_group(tg_meta)
  expect_length(parsed_group$tables, 1)
  
  # Verify that the table inherited the group's dialect (and its tab delimiter)
  tbl <- parsed_group$tables[[1]]
  expect_equal(tbl$dialect$delimiter, "\t")
  
  # Verify that the CSV parses correctly using the inherited dialect
  rows <- read_table_csv(tbl, strict = TRUE)
  expect_length(rows, 2)
  expect_equal(rows[[1]]$ID, "1")
  expect_equal(rows[[1]]$Name, "Alice")
})

test_that("utf-8-sig encoding is understood and mapped to UTF-8", {
  d <- parse_dialect(list(encoding = "utf-8-sig"))
  expect_equal(d$encoding, "UTF-8")
  
  d_upper <- parse_dialect(list(encoding = "UTF-8-SIG"))
  expect_equal(d_upper$encoding, "UTF-8")
})

test_that("header-only tables do not create bogus rows", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b", tmp)
  on.exit(unlink(tmp))

  rows <- read_table_csv(parse_table(list(url = tmp)))
  expect_length(rows, 0)
})

test_that("blank rows and comments follow dialect settings", {
  blank_file <- tempfile(fileext = ".csv")
  writeLines(c("value", "1", "", "2"), blank_file)
  on.exit(unlink(blank_file), add = TRUE)

  keep_blank <- parse_table(list(
    url = blank_file,
    dialect = list(skipBlankRows = FALSE)
  ))
  expect_length(read_table_csv(keep_blank), 3)

  comment_file <- tempfile(fileext = ".csv")
  writeLines(c("value", "# a comment", "1"), comment_file)
  on.exit(unlink(comment_file), add = TRUE)

  with_comments <- parse_table(list(
    url = comment_file,
    dialect = list(commentPrefix = "#")
  ))
  expect_length(read_table_csv(with_comments), 1)
})

