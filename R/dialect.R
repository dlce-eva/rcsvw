# CSVW Dialect Parsing and Representation

#' Create a CSVW Dialect object
#' @param commentPrefix character prefix for comments
#' @param delimiter character field separator
#' @param doubleQuote logical indicating whether quote chars are doubled
#' @param encoding character encoding
#' @param header logical indicating if file has header row
#' @param headerRowCount integer number of header rows
#' @param lineTerminators character vector of line terminators
#' @param quoteChar character used for quoting
#' @param skipBlankRows logical indicating if blank rows are skipped
#' @param skipColumns integer number of columns to skip at start of row
#' @param skipInitialSpace logical indicating if initial spaces are skipped
#' @param skipRows integer number of rows to skip at start of file
#' @param trim logical or character specifying how values are trimmed
#' @param ... other attributes
#' @return a csvw_dialect object
csvw_dialect <- function(
  commentPrefix = NULL,
  delimiter = ",",
  doubleQuote = TRUE,
  encoding = "utf-8",
  header = TRUE,
  headerRowCount = 1,
  lineTerminators = NULL,
  quoteChar = "\"",
  skipBlankRows = FALSE,
  skipColumns = 0,
  skipInitialSpace = FALSE,
  skipRows = 0,
  trim = TRUE,
  ...
) {
  if (!is.null(delimiter)) {
    if (identical(delimiter, "\\t")) delimiter <- "\t"
    else if (identical(delimiter, "\\n")) delimiter <- "\n"
    else if (identical(delimiter, "\\r")) delimiter <- "\r"
  }
  
  dialect <- list(
    commentPrefix = commentPrefix,
    delimiter = delimiter,
    doubleQuote = doubleQuote,
    encoding = encoding,
    header = header,
    headerRowCount = headerRowCount,
    lineTerminators = lineTerminators,
    quoteChar = quoteChar,
    skipBlankRows = skipBlankRows,
    skipColumns = skipColumns,
    skipInitialSpace = skipInitialSpace,
    skipRows = skipRows,
    trim = trim,
    ...
  )
  class(dialect) <- "csvw_dialect"
  dialect
}

validate_boolean <- function(val, name) {
  if (is.null(val)) return(TRUE)
  if (is.list(val) || length(val) != 1) stop(sprintf("%s must be a boolean", name))
  if (is.logical(val)) return(TRUE)
  if (is.character(val)) {
    if (val %in% c("true", "false")) return(TRUE)
  }
  stop(sprintf("%s must be a boolean", name))
}

validate_dialect_props <- function(d) {
  if (is.null(d)) return(TRUE)
  
  validate_boolean(d[["doubleQuote"]], "doubleQuote")
  validate_boolean(d[["header"]], "header")
  validate_boolean(d[["skipBlankRows"]], "skipBlankRows")
  validate_boolean(d[["skipInitialSpace"]], "skipInitialSpace")
  
  validate_non_negative_int <- function(val, name) {
    if (is.null(val)) return(TRUE)
    if (is.list(val) || length(val) != 1 || !is.numeric(val)) {
      stop(sprintf("%s must be a non-negative integer", name))
    }
    if (val %% 1 != 0 || val < 0) {
      stop(sprintf("%s must be a non-negative integer", name))
    }
    return(TRUE)
  }
  
  validate_non_negative_int(d[["headerRowCount"]], "headerRowCount")
  validate_non_negative_int(d[["skipColumns"]], "skipColumns")
  validate_non_negative_int(d[["skipRows"]], "skipRows")
  
  # commentPrefix
  cp <- d[["commentPrefix"]]
  if (!is.null(cp)) {
    if (is.list(cp) || !is.character(cp) || length(cp) != 1 || nchar(cp) != 1) {
      stop("commentPrefix must be a single character string")
    }
  }
  
  # delimiter
  delim <- d[["delimiter"]]
  if (!is.null(delim)) {
    if (identical(delim, "\\t")) delim <- "\t"
    else if (identical(delim, "\\n")) delim <- "\n"
    else if (identical(delim, "\\r")) delim <- "\r"
    if (is.list(delim) || !is.character(delim) || length(delim) != 1 || nchar(delim) != 1) {
      stop("delimiter must be a single character string")
    }
  }
  
  # quoteChar
  qchar <- d[["quoteChar"]]
  if (!is.null(qchar)) {
    if (is.list(qchar) || !is.character(qchar) || length(qchar) != 1 || (nchar(qchar) != 1 && qchar != "")) {
      stop("quoteChar must be a single character string or empty string")
    }
  }
  
  # lineTerminators
  lt <- d[["lineTerminators"]]
  if (!is.null(lt)) {
    if (!is.character(lt) && !is.list(lt)) {
      stop("lineTerminators must be a string or array of strings")
    }
    if (is.list(lt)) {
      for (item in lt) {
        if (!is.character(item)) stop("lineTerminators array elements must be strings")
      }
    }
  }
  
  # trim
  tr <- d[["trim"]]
  if (!is.null(tr)) {
    if (is.logical(tr)) {
      # OK
    } else if (is.character(tr) && length(tr) == 1) {
      if (!tr %in% c("true", "false", "start", "end")) {
        stop("trim must be a boolean or one of 'start', 'end'")
      }
    } else {
      stop("trim must be a boolean or one of 'start', 'end'")
    }
  }
  
  return(TRUE)
}

#' Parse Dialect list structure
#' @param d list or csvw_dialect object
#' @param fallback fallback dialect object
#' @param base_path base path/URL for resolving relative paths
#' @return a csvw_dialect object
parse_dialect <- function(d, fallback = NULL, base_path = "") {
  if (is.null(d)) {
    if (!is.null(fallback)) return(fallback)
    return(csvw_dialect())
  }
  if (is.character(d) && length(d) == 1) {
    resolved_url <- resolve_url(base_path, d)
    d <- get_json(resolved_url)
  }
  if (!is.list(d)) {
    stop("dialect must be an object or a URI string")
  }
  if (inherits(d, "csvw_dialect")) return(d)
  
  d_clean <- tryCatch({
    if (!is.null(d[["@type"]]) && d[["@type"]] != "Dialect") {
      stop("Dialect @type must be Dialect")
    }
    d_clean <- validate_object_properties(d, c("@type", "commentPrefix", "delimiter", "doubleQuote", "encoding", "header", "headerRowCount", "lineTerminators", "quoteChar", "skipBlankRows", "skipColumns", "skipInitialSpace", "skipRows", "trim"), is_description = FALSE, name = "Dialect")
    validate_dialect_props(d_clean)
    d_clean
  }, error = function(e) {
    warning(e$message)
    NULL
  })
  
  if (is.null(d_clean)) {
    if (!is.null(fallback)) return(fallback)
    return(csvw_dialect())
  }
  d <- d_clean
  
  get_val <- function(name, default_val) {
    if (!is.null(d[[name]])) return(d[[name]])
    if (!is.null(fallback) && !is.null(fallback[[name]])) return(fallback[[name]])
    return(default_val)
  }
  
  commentPrefix <- get_val("commentPrefix", NULL)
  delimiter <- get_val("delimiter", ",")
  doubleQuote <- as.logical(get_val("doubleQuote", TRUE))
  encoding <- get_val("encoding", "utf-8")
  if (tolower(encoding) == "utf-8-sig") {
    encoding <- "UTF-8"
  }
  header <- as.logical(get_val("header", TRUE))
  headerRowCount <- as.integer(get_val("headerRowCount", 1))
  lineTerminators <- get_val("lineTerminators", NULL)
  quoteChar <- get_val("quoteChar", "\"")
  skipBlankRows <- as.logical(get_val("skipBlankRows", FALSE))
  skipColumns <- as.integer(get_val("skipColumns", 0))
  skipInitialSpace <- as.logical(get_val("skipInitialSpace", FALSE))
  skipRows <- as.integer(get_val("skipRows", 0))
  trim <- get_val("trim", FALSE)
  
  if (is.character(trim)) {
    if (trim == "true") trim <- TRUE
    else if (trim == "false") trim <- FALSE
  }
  
  csvw_dialect(
    commentPrefix = commentPrefix,
    delimiter = delimiter,
    doubleQuote = doubleQuote,
    encoding = encoding,
    header = header,
    headerRowCount = headerRowCount,
    lineTerminators = lineTerminators,
    quoteChar = quoteChar,
    skipBlankRows = skipBlankRows,
    skipColumns = skipColumns,
    skipInitialSpace = skipInitialSpace,
    skipRows = skipRows,
    trim = trim
  )
}
