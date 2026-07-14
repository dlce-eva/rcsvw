# CSVW Datatypes parsing, formatting, and validation

# Built-in range limits for integer datatypes
INTEGER_RANGES <- list(
  unsignedInt = c(0, 4294967295),
  unsignedShort = c(0, 65535),
  unsignedLong = c(0, 18446744073709551615),
  unsignedByte = c(0, 255),
  short = c(-32768, 32767),
  long = c(-9223372036854775808, 9223372036854775807),
  byte = c(-128, 127),
  nonNegativeInteger = c(0, Inf),
  positiveInteger = c(1, Inf),
  nonPositiveInteger = c(-Inf, 0),
  negativeInteger = c(-Inf, -1)
)

BUILT_IN_DATATYPES <- c(
  "anyAtomicType", "anyURI", "base64Binary", "boolean", "date", "dateTime", "dateTimeStamp", "decimal", 
  "integer", "long", "int", "short", "byte", "nonNegativeInteger", "positiveInteger", 
  "unsignedLong", "unsignedInt", "unsignedShort", "unsignedByte", "nonPositiveInteger", 
  "negativeInteger", "double", "duration", "dayTimeDuration", "yearMonthDuration", "float", 
  "gDay", "gMonth", "gMonthDay", "gYear", "gYearMonth", "hexBinary", "QName", "string", 
  "normalizedString", "token", "language", "Name", "NCName", "NMTOKEN", "time",
  "number", "binary", "datetime", "json", "JSON", "xml", "html", "any"
)

is_builtin_datatype <- function(type_str) {
  clean_type <- type_str
  clean_type <- sub("^xsd:", "", clean_type)
  clean_type <- sub("^http://www.w3.org/2001/XMLSchema#", "", clean_type)
  clean_type <- sub("^csvw:", "", clean_type)
  clean_type <- sub("^http://www.w3.org/ns/csvw#", "", clean_type)
  
  return(clean_type %in% BUILT_IN_DATATYPES)
}

validate_numeric_pattern <- function(pat) {
  if (is.null(pat)) return(TRUE)
  if (!is.character(pat) || length(pat) != 1) {
    stop("numeric format pattern must be a single string")
  }
  if (!grepl("[#0]", pat)) {
    stop(paste("invalid numeric format pattern:", pat))
  }
  if (grepl("[\\[\\]\\{\\}]", pat)) {
    stop(paste("invalid numeric format pattern contains disallowed characters:", pat))
  }
  return(TRUE)
}

check_datetime_format_match <- function(v, format) {
  if (is.null(format) || !is.character(format)) return(TRUE)
  
  pat <- format
  pat <- gsub("yyyy", "@@YYYY@@", pat, fixed = TRUE)
  pat <- gsub("yy", "@@YY@@", pat, fixed = TRUE)
  pat <- gsub("MM", "@@MM_PAD@@", pat, fixed = TRUE)
  pat <- gsub("M", "@@MM@@", pat, fixed = TRUE)
  pat <- gsub("dd", "@@DD_PAD@@", pat, fixed = TRUE)
  pat <- gsub("d", "@@DD@@", pat, fixed = TRUE)
  pat <- gsub("HH", "@@HH_PAD@@", pat, fixed = TRUE)
  pat <- gsub("H", "@@HH@@", pat, fixed = TRUE)
  pat <- gsub("mm", "@@MIN_PAD@@", pat, fixed = TRUE)
  pat <- gsub("m", "@@MIN@@", pat, fixed = TRUE)
  pat <- gsub("ss", "@@SEC_PAD@@", pat, fixed = TRUE)
  pat <- gsub("s", "@@SEC@@", pat, fixed = TRUE)
  
  while (grepl("S", pat, fixed = TRUE)) {
    m <- regexpr("S+", pat)
    len <- attr(m, "match.length")
    placeholder <- sprintf("@@S_RUN_%d@@", len)
    pat <- sub("S+", placeholder, pat)
  }
  
  pat <- gsub("XXX", "@@TZ_COLON@@", pat, fixed = TRUE)
  pat <- gsub("XX", "@@TZ_NO_COLON@@", pat, fixed = TRUE)
  pat <- gsub("X", "@@TZ_SHORT@@", pat, fixed = TRUE)
  pat <- gsub("Z", "@@TZ_Z@@", pat, fixed = TRUE)
  
  pat <- gsub("([.\\+*?[^\\]${}()|])", "\\\\\\1", pat)
  
  pat <- gsub("@@YYYY@@", "[0-9]{4}", pat, fixed = TRUE)
  pat <- gsub("@@YY@@", "[0-9]{2}", pat, fixed = TRUE)
  pat <- gsub("@@MM_PAD@@", "[0-9]{2}", pat, fixed = TRUE)
  pat <- gsub("@@MM@@", "[0-9]{1,2}", pat, fixed = TRUE)
  pat <- gsub("@@DD_PAD@@", "[0-9]{2}", pat, fixed = TRUE)
  pat <- gsub("@@DD@@", "[0-9]{1,2}", pat, fixed = TRUE)
  pat <- gsub("@@HH_PAD@@", "[0-9]{2}", pat, fixed = TRUE)
  pat <- gsub("@@HH@@", "[0-9]{1,2}", pat, fixed = TRUE)
  pat <- gsub("@@MIN_PAD@@", "[0-9]{2}", pat, fixed = TRUE)
  pat <- gsub("@@MIN@@", "[0-9]{1,2}", pat, fixed = TRUE)
  pat <- gsub("@@SEC_PAD@@", "[0-9]{2}", pat, fixed = TRUE)
  pat <- gsub("@@SEC@@", "[0-9]{1,2}", pat, fixed = TRUE)
  
  while (grepl("@@S_RUN_", pat, fixed = TRUE)) {
    m <- regexpr("@@S_RUN_([0-9]+)@@", pat)
    match_str <- substring(pat, m, m + attr(m, "match.length") - 1)
    len <- as.integer(sub("@@S_RUN_([0-9]+)@@", "\\1", match_str))
    pat <- sub("@@S_RUN_[0-9]+@@", sprintf("[0-9]{%d}", len), pat)
  }
  
  pat <- gsub("@@TZ_COLON@@", "(?:Z|[+-][0-9]{2}:[0-9]{2})", pat, fixed = TRUE)
  pat <- gsub("@@TZ_NO_COLON@@", "(?:Z|[+-][0-9]{4})", pat, fixed = TRUE)
  pat <- gsub("@@TZ_SHORT@@", "(?:Z|[+-][0-9]{2})", pat, fixed = TRUE)
  pat <- gsub("@@TZ_Z@@", "(?:Z|[+-][0-9]{4})", pat, fixed = TRUE)
  
  pat <- paste0("^", pat, "$")
  
  if (!grepl(pat, v)) {
    stop(paste("Value", v, "does not match format pattern:", format))
  }
  return(TRUE)
}

is_valid_duration <- function(v) {
  if (!grepl("^-?P", v)) return(FALSE)
  if (grepl("T$", v) || v == "P" || v == "-P") return(FALSE)
  if (!grepl("^-?P(?:\\d+Y)?(?:\\d+M)?(?:\\d+D)?(?:T(?:\\d+H)?(?:\\d+M)?(?:\\d+(?:\\.\\d+)?S)?)?$", v)) return(FALSE)
  if (!grepl("[YMDHMS]", v)) return(FALSE)
  return(TRUE)
}

is_valid_day_time_duration <- function(v) {
  if (!grepl("^-?P", v)) return(FALSE)
  if (grepl("T$", v) || v == "P" || v == "-P") return(FALSE)
  if (!grepl("^-?P(?:\\d+D)?(?:T(?:\\d+H)?(?:\\d+M)?(?:\\d+(?:\\.\\d+)?S)?)?$", v)) return(FALSE)
  if (!grepl("[DHMS]", v)) return(FALSE)
  return(TRUE)
}

is_valid_year_month_duration <- function(v) {
  if (!grepl("^-?P", v)) return(FALSE)
  if (v == "P" || v == "-P") return(FALSE)
  if (!grepl("^-?P(?:\\d+Y)?(?:\\d+M)?$", v)) return(FALSE)
  if (!grepl("[YM]", v)) return(FALSE)
  return(TRUE)
}

validate_datatype_descriptor <- function(dt) {
  base_type <- dt[["base"]]
  
  id_val <- dt[["@id"]]
  if (!is.null(id_val)) {
    if (startsWith(id_val, "_:")) {
      stop("Datatype @id must not start with _:")
    }
    if (is_builtin_datatype(id_val)) {
      stop(paste("Datatype @id must not be a built-in datatype URL/name:", id_val))
    }
  }
  
  has_bounds <- !is.null(dt[["minimum"]]) || !is.null(dt[["minInclusive"]]) || !is.null(dt[["minExclusive"]]) ||
                !is.null(dt[["maximum"]]) || !is.null(dt[["maxInclusive"]]) || !is.null(dt[["maxExclusive"]])
  
  non_bound_types <- c("string", "normalizedString", "token", "language", "Name", "NCName", "NMTOKEN",
                       "boolean", "hexBinary", "base64Binary", "binary", "anyURI", "QName", "json", "JSON")
  
  if (has_bounds && base_type %in% non_bound_types) {
    stop(paste("Bounds constraints are not allowed on datatype:", base_type))
  }
  
  len <- dt[["length"]]
  min_len <- dt[["minLength"]]
  max_len <- dt[["maxLength"]]
  
  length_allowed_types <- c("string", "normalizedString", "token", "language", "Name", "NCName", "NMTOKEN",
                            "hexBinary", "base64Binary", "binary", "anyURI", "QName", "json", "JSON")
  if ((!is.null(len) || !is.null(min_len) || !is.null(max_len)) && !(base_type %in% length_allowed_types)) {
    stop(paste("Length constraints are not allowed on datatype:", base_type))
  }
  
  if (!is.null(len)) {
    if (!is.null(min_len) && len < min_len) {
      stop("length cannot be less than minLength")
    }
    if (!is.null(max_len) && len > max_len) {
      stop("length cannot be greater than maxLength")
    }
  }
  if (!is.null(min_len) && !is.null(max_len)) {
    if (max_len < min_len) {
      stop("maxLength cannot be less than minLength")
    }
  }
  
  min_inc <- dt[["minInclusive"]]
  min_exc <- dt[["minExclusive"]]
  max_inc <- dt[["maxInclusive"]]
  max_exc <- dt[["maxExclusive"]]
  
  if (!is.null(dt[["minimum"]])) min_inc <- dt[["minimum"]]
  if (!is.null(dt[["maximum"]])) max_inc <- dt[["maximum"]]
  
  if (!is.null(min_inc) && !is.null(min_exc)) {
    stop("Cannot specify both minInclusive and minExclusive")
  }
  if (!is.null(max_inc) && !is.null(max_exc)) {
    stop("Cannot specify both maxInclusive and maxExclusive")
  }
  
  parse_val <- function(b) {
    if (is.null(b)) return(NULL)
    if (is.numeric(b) || inherits(b, "Date") || inherits(b, "POSIXt")) return(b)
    tryCatch({
      parse_datatype_value(as.character(b), base_type)
    }, error = function(e) {
      stop(paste("Invalid bound value:", b, "-", e$message))
    })
  }
  
  val_min_inc <- parse_val(min_inc)
  val_min_exc <- parse_val(min_exc)
  val_max_inc <- parse_val(max_inc)
  val_max_exc <- parse_val(max_exc)
  
  if (!is.null(val_min_inc)) {
    if (!is.null(val_max_inc) && val_max_inc < val_min_inc) {
      stop("maxInclusive cannot be less than minInclusive")
    }
    if (!is.null(val_max_exc) && val_max_exc <= val_min_inc) {
      stop("maxExclusive must be greater than minInclusive")
    }
  }
  if (!is.null(val_min_exc)) {
    if (!is.null(val_max_inc) && val_max_inc <= val_min_exc) {
      stop("maxInclusive must be greater than minExclusive")
    }
    if (!is.null(val_max_exc) && val_max_exc <= val_min_exc) {
      stop("maxExclusive must be greater than minExclusive")
    }
  }
  
  # Format validation
  format <- dt[["format"]]
  if (!is.null(format)) {
    if (base_type %in% c("decimal", "integer", "long", "int", "short", "byte", "nonNegativeInteger", "positiveInteger",
                         "unsignedLong", "unsignedInt", "unsignedShort", "unsignedByte", "nonPositiveInteger", "negativeInteger",
                         "double", "float", "number", "numeric")) {
      if (is.character(format)) {
        validate_numeric_pattern(format)
      } else if (is.list(format)) {
        validate_object_properties(format, c("pattern", "groupChar", "decimalChar"), is_description = FALSE, name = "numeric format")
        
        dec_char <- format[["decimalChar"]]
        if (!is.null(dec_char)) {
          if (!is.character(dec_char) || length(dec_char) != 1 || nchar(dec_char) != 1) {
            stop("decimalChar must be a single character string")
          }
        }
        grp_char <- format[["groupChar"]]
        if (!is.null(grp_char)) {
          if (!is.character(grp_char) || length(grp_char) != 1 || nchar(grp_char) != 1) {
            stop("groupChar must be a single character string")
          }
        }
        pat <- format[["pattern"]]
        if (!is.null(pat)) {
          validate_numeric_pattern(pat)
        }
      } else {
        stop("numeric format must be a string or object")
      }
    } else if (base_type %in% c("date", "dateTime", "dateTimeStamp", "time", "gDay", "gMonth", "gMonthDay", "gYear", "gYearMonth")) {
      if (!is.character(format) || length(format) != 1) {
        stop("date/time format must be a single string")
      }
    } else if (base_type == "boolean") {
      if (!is.character(format) || length(format) != 1) {
        stop("boolean format must be a single string")
      }
      parts <- strsplit(format, "|", fixed = TRUE)[[1]]
      if (length(parts) != 2) {
        stop("boolean format must contain a single '|' separating true and false values")
      }
    } else {
      # For other datatypes (like string), format is a regular expression
      if (!is.character(format) || length(format) != 1) {
        stop("format must be a single string representing a regular expression")
      }
      is_valid_regex <- tryCatch({
        grepl(format, "")
        TRUE
      }, error = function(e) FALSE)
      if (!is_valid_regex) {
        stop(paste("Invalid regular expression in format:", format))
      }
    }
  }
}

#' Parse list structure or string to a Datatype definition
#' @param d character string or list representing datatype
#' @return datatype list structure
parse_datatype <- function(d) {
  if (is.null(d)) {
    return(list(base = "string"))
  }
  if (is.character(d)) {
    if (is_builtin_datatype(d)) {
      clean_type <- sub("^csvw:", "", d)
      clean_type <- sub("^http://www.w3.org/ns/csvw#", "", clean_type)
      return(list(base = clean_type))
    }
    if (is_url(d)) {
      dt_def <- tryCatch({
        get_json(d)
      }, error = function(e) {
        NULL
      })
      if (is.null(dt_def)) {
        warning(paste("Datatype URL does not resolve:", d))
        return(list(base = "string"))
      }
      return(parse_datatype(dt_def))
    }
    warning(paste("Invalid datatype:", d))
    return(list(base = "string"))
  }
  if (is.list(d)) {
    base_val <- d[["base"]]
    if (is.null(base_val)) {
      base_val <- "string"
      d[["base"]] <- "string"
    }
    if (!is_builtin_datatype(base_val)) {
      if (is_url(base_val)) {
        dt_def <- tryCatch({
          get_json(base_val)
        }, error = function(e) {
          NULL
        })
        if (is.null(dt_def)) {
          warning(paste("Base datatype URL does not resolve:", base_val))
          d[["base"]] <- "string"
        } else {
          parsed_base <- parse_datatype(dt_def)
          d[["base"]] <- parsed_base$base
        }
      } else {
        warning(paste("Invalid base datatype:", base_val))
        d[["base"]] <- "string"
      }
    } else {
      clean_base <- sub("^csvw:", "", base_val)
      clean_base <- sub("^http://www.w3.org/ns/csvw#", "", clean_base)
      d[["base"]] <- clean_base
    }
    validate_datatype_descriptor(d)
    return(d)
  }
  stop("Invalid value for Datatype")
}

#' Parse a raw cell string into R native type based on datatype rules
#' @param v raw cell character string
#' @param dt parsed datatype structure
#' @param strict logical, if TRUE raises error on invalid value, otherwise issues warning
#' @param validate logical, if TRUE performs cell constraints validation
#' @return parsed R value (numeric, logical, Date, POSIXct, or character)
parse_cell <- function(v, dt, strict = TRUE, validate = TRUE) {
  if (is.null(v) || is.na(v)) return(NULL)
  
  base_type <- dt[["base"]]
  format <- dt[["format"]]
  
  # Parse according to base type
  parsed_val <- tryCatch({
    parse_datatype_value(v, base_type, format)
  }, error = function(e) {
    if (strict) {
      stop(e$message)
    } else {
      warning(paste("Invalid column value:", v, ";", e$message))
      return(v)
    }
  })
  
  # Validate datatype constraints if parsing was successful
  if (validate && !is.null(parsed_val)) {
    tryCatch({
      validate_constraints(parsed_val, dt)
    }, error = function(e) {
      if (strict) {
        stop(e$message)
      } else {
        warning(paste("Constraint violation:", v, ";", e$message))
      }
    })
  }
  
  return(parsed_val)
}

#' Helper to parse base datatype value
#' @param v character string
#' @param base_type base datatype name
#' @param format optional format specification
#' @return parsed R value
parse_datatype_value <- function(v, base_type, format = NULL) {
  if (is.null(v) || is.na(v) || v == "") return(NULL)
  
  # Boolean
  if (base_type == "boolean") {
    v_clean <- trimws(v)
    true_vals <- c("true", "1")
    false_vals <- c("false", "0")
    if (!is.null(format) && is.character(format) && grepl("\\|", format)) {
      parts <- strsplit(format, "|", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        true_vals <- parts[1]
        false_vals <- parts[2]
      }
    }
    if (v_clean %in% true_vals) {
      return(TRUE)
    } else if (v_clean %in% false_vals) {
      return(FALSE)
    } else {
      stop(paste("invalid lexical value for boolean:", v))
    }
  }
  
  # Decimal and Number types
  if (base_type %in% c("decimal", "number", "float", "double")) {
    return(parse_decimal(v, format, base_type = base_type))
  }
  
  # Integers
  if (base_type == "integer" || base_type == "int" || base_type %in% names(INTEGER_RANGES)) {
    val <- parse_decimal(v, format, base_type = "integer")
    if (val %% 1 != 0) {
      stop(paste("value must be an integer, but got:", val))
    }
    
    # Range checks
    if (base_type %in% names(INTEGER_RANGES)) {
      limits <- INTEGER_RANGES[[base_type]]
      if (val < limits[1] || val > limits[2]) {
        stop(paste(base_type, "must be an integer between", limits[1], "and", limits[2]))
      }
    }
    return(as.integer(val))
  }
  
  # Date / Time / DateTime
  if (base_type %in% c("date", "dateTime", "dateTimeStamp", "datetime", "time")) {
    return(parse_datetime(v, format, base_type))
  }
  
  # Durations
  if (base_type == "duration") {
    if (!is_valid_duration(v)) stop(paste("invalid duration format:", v))
  } else if (base_type == "dayTimeDuration") {
    if (!is_valid_day_time_duration(v)) stop(paste("invalid dayTimeDuration format:", v))
  } else if (base_type == "yearMonthDuration") {
    if (!is_valid_year_month_duration(v)) stop(paste("invalid yearMonthDuration format:", v))
  }
  
  # Default fallback is character/string, check format regex constraints if present
  if (!is.null(format) && is.character(format)) {
    pattern <- format
    if (!startsWith(pattern, "^")) pattern <- paste0("^", pattern)
    if (!endsWith(pattern, "$")) pattern <- paste0(pattern, "$")
    if (!grepl(pattern, v)) {
      stop(paste("Value", v, "does not match format pattern", format))
    }
  }
  return(v)
}

parse_pattern_constraints <- function(pattern) {
  if (is.null(pattern) || pattern == "") return(NULL)
  
  m <- regexpr("[#0][#0,.]*[#0]?", pattern)
  if (m == -1) return(NULL)
  num_part <- substring(pattern, m, m + attr(m, "match.length") - 1)
  
  parts <- strsplit(num_part, ".", fixed = TRUE)[[1]]
  int_part <- parts[1]
  frac_part <- if (length(parts) > 1) parts[2] else ""
  
  has_group <- grepl(",", int_part, fixed = TRUE)
  group_size <- 0
  secondary_group_size <- 0
  if (has_group) {
    subparts <- strsplit(int_part, ",", fixed = TRUE)[[1]]
    group_size <- nchar(subparts[length(subparts)])
    secondary_group_size <- if (length(subparts) >= 3) nchar(subparts[length(subparts) - 1]) else group_size
  }
  
  min_int_digits <- nchar(gsub("[^0-9]", "", int_part))
  
  min_frac_digits <- 0
  max_frac_digits <- 0
  if (frac_part != "") {
    frac_clean <- gsub(",", "", frac_part, fixed = TRUE)
    min_frac_digits <- nchar(gsub("[^0-9]", "", frac_clean))
    max_frac_digits <- nchar(gsub("[^#0]", "", frac_clean))
  }
  
  has_frac_group <- if (frac_part != "") grepl(",", frac_part, fixed = TRUE) else FALSE
  frac_group_size <- 0
  if (has_frac_group) {
    subparts <- strsplit(frac_part, ",", fixed = TRUE)[[1]]
    frac_group_size <- nchar(subparts[1])
  }
  
  return(list(
    has_group = has_group,
    group_size = group_size,
    secondary_group_size = secondary_group_size,
    min_int_digits = min_int_digits,
    min_frac_digits = min_frac_digits,
    max_frac_digits = max_frac_digits,
    has_frac_group = has_frac_group,
    frac_group_size = frac_group_size
  ))
}

#' Parse decimal values from string supporting group and decimal characters
#' @param v raw string
#' @param format format list/string
#' @param base_type base datatype name
#' @return numeric
parse_decimal <- function(v, format, base_type = "decimal") {
  v_clean <- trimws(v)
  if (v_clean == "") return(NA)
  
  pattern <- NULL
  groupChar <- ","
  decimalChar <- "."
  
  if (is.character(format) && length(format) == 1) {
    pattern <- format
  } else if (is.list(format)) {
    pattern <- format[["pattern"]]
    if (!is.null(format[["groupChar"]])) groupChar <- format[["groupChar"]]
    if (!is.null(format[["decimalChar"]])) decimalChar <- format[["decimalChar"]]
  }
  
  if (!is.null(groupChar) && groupChar != "") {
    double_group <- paste0(groupChar, groupChar)
    if (grepl(double_group, v_clean, fixed = TRUE)) {
      stop(paste("value contains consecutive grouping characters:", double_group))
    }
  }
  
  # Special values for number, float, double
  if (base_type %in% c("double", "float", "number", "numeric")) {
    if (v_clean == "NaN") return(NaN)
    if (v_clean %in% c("INF", "+INF")) return(Inf)
    if (v_clean == "-INF") return(-Inf)
  }
  
  # Scientific notation check
  if (grepl("[eE]", v_clean)) {
    if (!base_type %in% c("double", "float", "number", "numeric")) {
      stop(paste("Scientific notation not allowed for datatype:", base_type))
    }
    
    e_parts <- strsplit(v_clean, "[eE]")[[1]]
    if (length(e_parts) != 2) stop("Invalid scientific notation")
    
    exp_part <- e_parts[2]
    if (!grepl("^[+-]?[0-9]+$", exp_part)) stop("Invalid scientific notation exponent")
    
    mantissa <- e_parts[1]
    mantissa_val <- parse_decimal(mantissa, format, base_type = "decimal")
    
    exponent_val <- as.numeric(exp_part)
    return(mantissa_val * (10 ^ exponent_val))
  }
  
  has_minus <- FALSE
  has_plus <- FALSE
  percent <- FALSE
  permille <- FALSE
  
  if (grepl("%", v_clean)) {
    percent <- TRUE
    v_clean <- gsub("%", "", v_clean, fixed = TRUE)
  }
  if (grepl("\u2030", v_clean)) {
    permille <- TRUE
    v_clean <- gsub("\u2030", "", v_clean, fixed = TRUE)
  }
  
  v_clean <- trimws(v_clean)
  
  if (startsWith(v_clean, "-")) {
    has_minus <- TRUE
    v_clean <- substring(v_clean, 2)
  } else if (startsWith(v_clean, "+")) {
    has_plus <- TRUE
    v_clean <- substring(v_clean, 2)
  }
  
  v_clean <- trimws(v_clean)
  
  esc_group <- if (groupChar == "]") "\\]" else if (groupChar %in% c("\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "{")) paste0("\\", groupChar) else groupChar
  esc_dec <- if (decimalChar == "]") "\\]" else if (decimalChar %in% c("\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "{")) paste0("\\", decimalChar) else decimalChar
  
  allowed_chars_regex <- paste0("^[0-9", esc_group, esc_dec, "]*$")
  if (!grepl(allowed_chars_regex, v_clean)) {
    stop(paste("invalid lexical value for decimal:", v))
  }
  
  val_parts <- strsplit(v_clean, decimalChar, fixed = TRUE)[[1]]
  if (length(val_parts) > 2) stop("Multiple decimal characters in value")
  val_int <- val_parts[1]
  val_frac <- if (length(val_parts) > 1) val_parts[2] else ""
  
  if (!is.null(pattern)) {
    constraints <- parse_pattern_constraints(pattern)
    if (!is.null(constraints)) {
      pattern_has_percent <- grepl("%", pattern, fixed = TRUE)
      pattern_has_permille <- grepl("\u2030", pattern, fixed = TRUE)
      if (pattern_has_percent && !percent) stop("Missing percent sign")
      if (!pattern_has_percent && percent) stop("Unexpected percent sign")
      if (pattern_has_permille && !permille) stop("Missing permille sign")
      if (!pattern_has_permille && permille) stop("Unexpected permille sign")
      
      pattern_has_decimal <- grepl(".", pattern, fixed = TRUE)
      if (length(val_parts) > 1 && !pattern_has_decimal) {
        stop("Decimal character present in value but not allowed by pattern")
      }
      
      if (constraints$has_group) {
        int_subparts <- strsplit(val_int, groupChar, fixed = TRUE)[[1]]
        n_sub <- length(int_subparts)
        if (n_sub > 1) {
          if (nchar(int_subparts[n_sub]) != constraints$group_size) {
            stop("Incorrect grouping separator position in value")
          }
          if (n_sub > 2) {
            for (k in 2:(n_sub - 1)) {
              if (nchar(int_subparts[k]) != constraints$secondary_group_size) {
                stop("Incorrect grouping separator position in value")
              }
            }
          }
          if (nchar(int_subparts[1]) < 1 || nchar(int_subparts[1]) > constraints$secondary_group_size) {
            stop("Incorrect grouping separator position in value")
          }
        } else {
          if (nchar(val_int) > constraints$group_size) {
            stop("Value is longer than group size but missing group separators")
          }
        }
      } else {
        if (grepl(groupChar, val_int, fixed = TRUE)) {
          stop("Group separator present in value but not allowed by pattern")
        }
      }
      
      int_digits <- gsub("[^0-9]", "", val_int)
      if (nchar(int_digits) < constraints$min_int_digits) {
        stop("Too few integer digits in value")
      }
      
      if (constraints$has_frac_group) {
        frac_subparts <- strsplit(val_frac, groupChar, fixed = TRUE)[[1]]
        n_frac_sub <- length(frac_subparts)
        if (n_frac_sub > 1) {
          for (k in 1:(n_frac_sub - 1)) {
            if (nchar(frac_subparts[k]) != constraints$frac_group_size) {
              stop("Incorrect fractional grouping separator position")
            }
          }
          if (nchar(frac_subparts[n_frac_sub]) < 1 || nchar(frac_subparts[n_frac_sub]) > constraints$frac_group_size) {
            stop("Incorrect fractional grouping separator position")
          }
        } else {
          if (nchar(val_frac) > constraints$frac_group_size) {
            stop("Fractional part is longer than group size but missing group separators")
          }
        }
      } else {
        if (grepl(groupChar, val_frac, fixed = TRUE)) {
          stop("Group separator present in fractional part but not allowed by pattern")
        }
      }
      
      frac_digits_count <- nchar(gsub("[^0-9]", "", val_frac))
      if (frac_digits_count < constraints$min_frac_digits) {
        stop("Too few fractional digits")
      }
      if (frac_digits_count > constraints$max_frac_digits) {
        stop("Too many fractional digits")
      }
    }
  } else {
    if (grepl(groupChar, val_frac, fixed = TRUE)) {
      stop("Group separator present in fractional part")
    }
  }
  
  v_standard <- paste0(
    if (has_minus) "-" else "",
    gsub("[^0-9]", "", val_int),
    if (length(val_parts) > 1) "." else "",
    gsub("[^0-9]", "", val_frac)
  )
  
  val <- as.numeric(v_standard)
  if (is.na(val)) {
    stop(paste("invalid lexical value for decimal:", v))
  }
  
  if (percent) val <- val * 0.01
  if (permille) val <- val * 0.001
  
  return(val)
}

#' Parse datetime formats
#' @param v raw string
#' @param format format pattern
#' @param base_type date or datetime base type
#' @return Date or POSIXct
parse_datetime <- function(v, format, base_type) {
  v_clean <- trimws(v)
  v_clean <- sub("([+-])([0-9]{2}):([0-9]{2})$", "\\1\\2\\3", v_clean)
  v_clean <- sub("Z$", "+0000", v_clean)
  
  r_format <- NULL
  if (!is.null(format) && is.character(format)) {
    r_format <- format
    r_format <- gsub("yyyy", "@@1@@", r_format)
    r_format <- gsub("yy", "@@2@@", r_format)
    r_format <- gsub("MM", "@@3@@", r_format)
    r_format <- gsub("M", "@@3@@", r_format)
    r_format <- gsub("dd", "@@4@@", r_format)
    r_format <- gsub("d", "@@4@@", r_format)
    r_format <- gsub("HH", "@@5@@", r_format)
    r_format <- gsub("H", "@@5@@", r_format)
    r_format <- gsub("mm", "@@6@@", r_format)
    r_format <- gsub("m", "@@6@@", r_format)
    r_format <- gsub("ss[.,]S+", "@@8@@", r_format)
    r_format <- gsub("ss[.,]S", "@@8@@", r_format)
    r_format <- gsub("ss", "@@7@@", r_format)
    r_format <- gsub("s", "@@7@@", r_format)
    r_format <- gsub("S+", "", r_format)
    r_format <- gsub("X+", "%z", r_format)
    r_format <- gsub("Z", "%z", r_format)
    
    r_format <- gsub("@@1@@", "%Y", r_format)
    r_format <- gsub("@@2@@", "%y", r_format)
    r_format <- gsub("@@3@@", "%m", r_format)
    r_format <- gsub("@@4@@", "%d", r_format)
    r_format <- gsub("@@5@@", "%H", r_format)
    r_format <- gsub("@@6@@", "%M", r_format)
    r_format <- gsub("@@8@@", "%OS", r_format)
    r_format <- gsub("@@7@@", "%S", r_format)
  }
  
  if (base_type == "date") {
    if (is.null(r_format)) r_format <- "%Y-%m-%d"
    res_posix <- tryCatch({
      strptime(v_clean, format = r_format, tz = "UTC")
    }, error = function(e) NA)
    if (is.null(res_posix) || all(is.na(res_posix))) {
      res <- as.Date(v_clean)
    } else {
      res <- as.Date(res_posix)
    }
    if (is.na(res)) stop(paste("invalid lexical value for date:", v))
    return(res)
  } else if (base_type %in% c("dateTime", "dateTimeStamp", "datetime")) {
    if (is.null(r_format)) {
      res <- as.POSIXct(strptime(v_clean, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"), tz = "UTC")
      if (is.na(res)) res <- as.POSIXct(strptime(v_clean, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), tz = "UTC")
      if (is.na(res)) res <- as.POSIXct(strptime(v_clean, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"), tz = "UTC")
      if (is.na(res)) res <- as.POSIXct(strptime(v_clean, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"), tz = "UTC")
    } else {
      res <- as.POSIXct(strptime(v_clean, format = r_format, tz = "UTC"), tz = "UTC")
    }
    if (is.na(res)) stop(paste("invalid lexical value for dateTime:", v))
    return(res)
  } else if (base_type == "time") {
    return(v_clean)
  }
  return(v_clean)
}

#' Validate datatype constraints
#' @param val parsed R value
#' @param dt datatype definition list
#' @return logical TRUE if constraints met, errors otherwise
validate_constraints <- function(val, dt) {
  base_type <- dt[["base"]]
  val_len <- if (is.character(val)) {
    if (identical(base_type, "hexBinary")) {
      nchar(val) / 2
    } else if (identical(base_type, "base64Binary")) {
      s_clean <- gsub("[ \t\r\n]", "", val)
      n <- nchar(s_clean)
      p <- 0
      if (endsWith(s_clean, "==")) {
        p <- 2
      } else if (endsWith(s_clean, "=")) {
        p <- 1
      }
      (n * 3 / 4) - p
    } else {
      nchar(val)
    }
  } else {
    length(val)
  }
  
  length_limit <- dt[["length"]]
  minLength <- dt[["minLength"]]
  maxLength <- dt[["maxLength"]]
  
  if (!is.null(length_limit) && val_len != length_limit) {
    stop(paste("value must have length", length_limit))
  }
  if (!is.null(minLength) && val_len < minLength) {
    stop(paste("value must have at least length", minLength))
  }
  if (!is.null(maxLength) && val_len > maxLength) {
    stop(paste("value must have at most length", maxLength))
  }
  
  # Regex pattern validation
  pattern_regex <- dt[["pattern"]]
  if (!is.null(pattern_regex) && is.character(pattern_regex) && pattern_regex != "") {
    anchored_pattern <- paste0("^", pattern_regex, "$")
    if (!grepl(anchored_pattern, as.character(val))) {
      stop(paste("value does not match pattern:", pattern_regex))
    }
  }
  
  # Bounds
  minimum <- dt[["minimum"]]
  minInclusive <- dt[["minInclusive"]]
  minExclusive <- dt[["minExclusive"]]
  maximum <- dt[["maximum"]]
  maxInclusive <- dt[["maxInclusive"]]
  maxExclusive <- dt[["maxExclusive"]]
  
  base_dt <- dt[["base"]]
  
  parse_bound <- function(b, b_type) {
    if (is.null(b)) return(NULL)
    if (is.numeric(b) || inherits(b, "Date") || inherits(b, "POSIXt")) return(b)
    parse_datatype_value(as.character(b), b_type)
  }
  
  # Min checks
  min_val <- parse_bound(minimum, base_dt)
  if (!is.null(min_val) && val < min_val) {
    stop(paste("value must be >=", min_val))
  }
  min_inc <- parse_bound(minInclusive, base_dt)
  if (!is.null(min_inc) && val < min_inc) {
    stop(paste("value must be >=", min_inc))
  }
  min_exc <- parse_bound(minExclusive, base_dt)
  if (!is.null(min_exc) && val <= min_exc) {
    stop(paste("value must be >", min_exc))
  }
  
  # Max checks
  max_val <- parse_bound(maximum, base_dt)
  if (!is.null(max_val) && val > max_val) {
    stop(paste("value must be <=", max_val))
  }
  max_inc <- parse_bound(maxInclusive, base_dt)
  if (!is.null(max_inc) && val > max_inc) {
    stop(paste("value must be <=", max_inc))
  }
  max_exc <- parse_bound(maxExclusive, base_dt)
  if (!is.null(max_exc) && val >= max_exc) {
    stop(paste("value must be <", max_exc))
  }
  
  return(TRUE)
}
