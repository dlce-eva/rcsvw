library(testthat)

test_that("boolean datatype parsing works", {
  # Default boolean formats
  dt <- parse_datatype("boolean")
  expect_true(parse_cell("true", dt))
  expect_true(parse_cell("1", dt))
  expect_false(parse_cell("false", dt))
  expect_false(parse_cell("0", dt))
  expect_error(parse_cell("invalid", dt))
  
  # Custom boolean formats
  dt_custom <- parse_datatype(list(base = "boolean", format = "Yea|Nay"))
  expect_true(parse_cell("Yea", dt_custom))
  expect_false(parse_cell("Nay", dt_custom))
  expect_error(parse_cell("true", dt_custom))
})

test_that("numeric and decimal datatype parsing works", {
  dt <- parse_datatype("decimal")
  expect_equal(parse_cell("123.45", dt), 123.45)
  expect_equal(parse_cell("-12.3", dt), -12.3)
  
  # Group and decimal char formatting
  dt_fmt <- parse_datatype(list(base = "decimal", format = list(groupChar = ".", decimalChar = ",")))
  expect_equal(parse_cell("1.234,5", dt_fmt), 1234.5)
  
  # Percent and permille
  dt_pct <- parse_datatype("decimal")
  expect_equal(parse_cell("15%", dt_pct), 0.15)
  expect_equal(parse_cell("15‰", dt_pct), 0.015)
})

test_that("integer range constraints work", {
  dt_byte <- parse_datatype("byte")
  expect_equal(parse_cell("100", dt_byte), 100)
  expect_error(parse_cell("200", dt_byte)) # Exceeds range limit of byte [-128, 127]
  expect_error(parse_cell("123.45", dt_byte)) # Not a whole number
  
  dt_unsigned <- parse_datatype("unsignedShort")
  expect_equal(parse_cell("65530", dt_unsigned), 65530)
  expect_error(parse_cell("-5", dt_unsigned))

  # Values wider than R's signed 32-bit integer must not silently become NA.
  dt_unsigned_int <- parse_datatype("unsignedInt")
  expect_equal(parse_cell("4294967295", dt_unsigned_int), 4294967295)

  # Values wider than an exactly representable R double are retained as text.
  dt_unsigned_long <- parse_datatype("unsignedLong")
  expect_equal(parse_cell("18446744073709551615", dt_unsigned_long), "18446744073709551615")
  expect_error(parse_cell("18446744073709551616", dt_unsigned_long))

  dt_constrained_long <- parse_datatype(list(
    base = "unsignedLong", minInclusive = "18446744073709551614"
  ))
  expect_equal(
    parse_cell("18446744073709551615", dt_constrained_long),
    "18446744073709551615"
  )
  expect_error(parse_cell("18446744073709551613", dt_constrained_long))
})

test_that("date and datetime parsing works", {
  dt_date <- parse_datatype("date")
  expect_equal(as.character(parse_cell("2020-05-15", dt_date)), "2020-05-15")
  
  dt_datetime <- parse_datatype("dateTime")
  expect_s3_class(parse_cell("2020-05-15T10:20:30", dt_datetime), "POSIXt")
})

test_that("constraints check validation works", {
  dt_str <- parse_datatype(list(base = "string", minLength = 3, maxLength = 5))
  expect_equal(parse_cell("abcd", dt_str), "abcd")
  expect_error(parse_cell("ab", dt_str))
  expect_error(parse_cell("abcdef", dt_str))
  
  dt_range <- parse_datatype(list(base = "integer", minimum = 10, maximum = 20))
  expect_equal(parse_cell("15", dt_range), 15)
  expect_error(parse_cell("5", dt_range))
  expect_error(parse_cell("25", dt_range))
})

test_that("json datatype parsing works and returns data as plain string", {
  dt_json <- parse_datatype("json")
  expect_equal(dt_json$base, "json")
  expect_equal(parse_cell('{"a": 1}', dt_json), '{"a": 1}')
  
  dt_json_upper <- parse_datatype("JSON")
  expect_equal(dt_json_upper$base, "JSON")
  expect_equal(parse_cell('[1, 2, 3]', dt_json_upper), '[1, 2, 3]')
})
