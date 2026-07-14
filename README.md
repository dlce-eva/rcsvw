# rcsvw
R package implementing the CSVW spec

`rcsvw` is an R package for reading, writing, and validating CSVW (CSV on the Web) metadata and datasets. It is fully compliant with the W3C CSV on the Web specifications, converting CSV tables described by JSON-LD metadata into standardized R `data.frame` objects, JSON serialization, and validating constraints.


## Overview

### Exploring CSVW described data

To load a CSVW described dataset, you can read the metadata directly from a local path or a remote URL. The `csvw()` function parses both the metadata and the actual CSV tabular data, returning a structured `csvw` object.

```R
library(rcsvw)

# Load CSVW from a metadata URL/path
res <- csvw("https://raw.githubusercontent.com/cldf/csvw/master/tests/fixtures/csv.txt-metadata.json")

# Explore the tables
length(res$tables)
# [1] 1

# Check the schema columns and datatypes
first_col <- res$tables[[1]]$tableSchema$columns[[1]]
first_col$name
# [1] "ID"
first_col$datatype$base
# [1] "string"

# Convert a table to a standard R data.frame
df <- as.data.frame(res$tables[[1]])
head(df)
```


If you only want the data frames and don't need the metadata structure, you can use the convenient `read_csvw()` helper:

```R
# Reads data directly to a data.frame (or list of data.frames for table groups)
df <- read_csvw("https://raw.githubusercontent.com/cldf/csvw/master/tests/fixtures/csv.txt-metadata.json")
```


### Creating and Writing CSVW described data

You can create table schemas in R and export the data along with its CSVW JSON metadata using `write_csvw()`.

```R
# Define a table group or table structure in R
col1 <- parse_column(list(name = "id", datatype = "integer"))
col2 <- parse_column(list(name = "name", datatype = "string"))

tbl_schema <- list(
  columns = list(col1, col2),
  primaryKey = "id"
)
class(tbl_schema) <- "csvw_table_schema"

# Define table metadata
table_desc <- list(
  url = "output.csv",
  tableSchema = tbl_schema,
  data = list(
    list(id = 1, name = "Item A"),
    list(id = 2, name = "Item B")
  )
)
class(table_desc) <- "csvw_table"

# Write the data to output.csv and metadata to output.json
write_csvw(table_desc, "output.json")
```


### Where's the "on the Web" part?

CSVW includes a specification for [locating metadata](https://www.w3.org/TR/tabular-data-model/#locating-metadata). If you find the CSV data first, the parser can automatically discover and locate the matching schema metadata file relative to it.

```R
# Load directly from the CSV URL. rcsvw will discover and download the corresponding metadata JSON.
data <- csvw("https://raw.githubusercontent.com/cldf/csvw/master/tests/fixtures/csv.txt")

# The schema table group has been automatically located and parsed
data$t
```


## Top-Level Functions Reference

### `csvw(url, md_url = NULL, validate = FALSE, lax = FALSE)`
R API to read CSVW described data and resolve metadata.

**Parameters**:
- `url`: Path or URL to the CSV file or the metadata JSON file.
- `md_url`: Optional explicit path or URL to the metadata JSON file.
- `validate`: If `TRUE`, validates primary keys and referential integrity.
- `lax`: If `TRUE`, ignores warnings/errors in metadata properties that do not strictly comply.

**Returns**:
A list structure of class `csvw` containing:
- `t`: The parsed `csvw_table_group` or `csvw_table`.
- `tables`: A list of `csvw_table` objects.
- `is_valid`: Boolean indicating if validation succeeded.
- `warnings`: Character vector of warnings during parsing.


### `read_csvw(url, md_url = NULL)`
Helper function to directly load a CSVW described dataset and return its tabular data as standard R `data.frame` objects.

**Returns**:
A single `data.frame` (for tables) or a named list of `data.frame` objects (for table groups).


### `validate_csvw(url, md_url = NULL, lax = FALSE)`
Helper function to validate metadata structures and column cells against datatype constraints.

**Returns**:
Logical `TRUE` if the dataset is valid, `FALSE` otherwise.


### `write_csvw(tg, fname, strict = FALSE, ...)`
Writes table data to CSV files and serializes the CSVW schema properties to a JSON-LD file.

**Parameters**:
- `tg`: A `csvw`, `csvw_table_group`, or `csvw_table` object.
- `fname`: Filename where the JSON metadata should be written.
- `strict`: If `TRUE`, throws an error if data violates constraints.


### S3 Methods

#### `as.data.frame(x, ...)`
Converts a `csvw_table` or `csvw_table_group` object into R `data.frame` format, preserving column types (integer, decimal, logical, Date, POSIXct).


