# S3 classes for CSVW Metadata: Column, TableSchema, Table, TableGroup, and CSVW

get_first_title <- function(titles) {
  if (is.null(titles) || length(titles) == 0) return(NULL)
  first <- titles[[1]]
  while (is.list(first) && length(first) > 0) {
    if (!is.null(first[["@value"]])) {
      return(as.character(first[["@value"]]))
    }
    first <- first[[1]]
  }
  if (is.null(first) || length(first) == 0) return(NULL)
  return(as.character(first))
}

check_id <- function(id_val) {
  if (!is.null(id_val)) {
    if (!is.character(id_val) || length(id_val) != 1 || startsWith(id_val, "_:")) {
      stop("Invalid @id: must be a single string URI and not a blank node")
    }
  }
}

check_type <- function(type_val, expected) {
  if (!is.null(type_val)) {
    types <- unlist(type_val)
    allowed <- c(expected, paste0("csvw:", expected), paste0("http://www.w3.org/ns/csvw#", expected))
    if (!any(types %in% allowed)) {
      stop(sprintf("Invalid @type: must be %s", expected))
    }
  }
}

validate_bcp47 <- function(tag) {
  if (is.null(tag)) return(TRUE)
  if (!is.character(tag) || length(tag) != 1) return(FALSE)
  if (!grepl("^[a-zA-Z0-9-]+$", tag)) return(FALSE)
  parts <- strsplit(tag, "-", fixed = TRUE)[[1]]
  if (length(parts) == 0) return(FALSE)
  first_len <- nchar(parts[1])
  if (first_len < 2 || first_len > 8) {
    if (tolower(parts[1]) != "x" && tolower(parts[1]) != "i") {
      return(FALSE)
    }
  }
  if (length(parts) > 1) {
    for (p in parts[-1]) {
      if (nchar(p) < 1 || nchar(p) > 8) return(FALSE)
    }
  }
  return(TRUE)
}

validate_natural_language_property <- function(x, name = "property") {
  if (is.null(x)) return(TRUE)
  
  validate_nl_object <- function(obj) {
    if (!is.list(obj)) stop(paste(name, "must be an object or a string"))
    keys <- names(obj)
    for (k in keys) {
      if (startsWith(k, "@") && !k %in% c("@value", "@language", "@type")) {
        stop(paste(name, "contains invalid keyword:", k))
      }
    }
    if ("@value" %in% keys) {
      extra_keys <- setdiff(keys, c("@value", "@language", "@type"))
      if (length(extra_keys) > 0) {
        stop(paste(name, "object with @value contains extra properties:", paste(extra_keys, collapse=", ")))
      }
      if (!is.character(obj[["@value"]])) {
        stop(paste(name, "@value must be a string"))
      }
      if ("@language" %in% keys) {
        lang <- obj[["@language"]]
        if (!is.character(lang) || length(lang) != 1) stop(paste(name, "@language must be a string"))
        if (!validate_bcp47(lang)) stop(paste(name, "invalid @language tag:", lang))
      }
      if ("@type" %in% keys) {
        if (!is.character(obj[["@type"]])) stop(paste(name, "@type must be a string"))
      }
    } else {
      if ("@language" %in% keys) {
        stop(paste(name, "@language cannot appear outside of an object with @value"))
      }
      if ("@type" %in% keys) {
        stop(paste(name, "@type cannot appear outside of an object with @value"))
      }
      for (lang in keys) {
        if (!validate_bcp47(lang)) {
          stop(paste(name, "invalid language tag in map:", lang))
        }
        val <- obj[[lang]]
        if (!is.character(val) && !is.list(val)) {
          stop(paste(name, "values in language map must be strings or arrays of strings"))
        }
        if (is.list(val)) {
          for (item in val) {
            if (!is.character(item)) stop(paste(name, "array elements in language map must be strings"))
          }
        }
      }
    }
  }

  if (is.character(x)) {
    return(TRUE)
  }
  
  if (is.list(x)) {
    if (is.null(names(x))) {
      for (item in x) {
        if (is.character(item)) {
          # OK
        } else if (is.list(item)) {
          validate_nl_object(item)
        } else {
          stop(paste(name, "array elements must be strings or objects"))
        }
      }
    } else {
      validate_nl_object(x)
    }
  } else {
    stop(paste(name, "must be a string, array, or object"))
  }
  return(TRUE)
}

check_prop <- function(val, validation_fn, default_val = NULL) {
  if (is.null(val)) return(default_val)
  res <- tryCatch({
    validation_fn(val)
    TRUE
  }, error = function(e) {
    warning(e$message)
    FALSE
  })
  if (res) val else default_val
}

validate_titles <- function(titles) {
  validate_natural_language_property(titles, "titles")
}

validate_null_property <- function(null_val) {
  if (is.null(null_val)) return(TRUE)
  if (!is.character(null_val) && !is.list(null_val)) {
    stop("null property must be a string or an array of strings")
  }
  if (is.list(null_val)) {
    for (item in null_val) {
      if (!is.character(item)) {
        stop("null property array elements must be strings")
      }
    }
  }
  return(TRUE)
}

validate_lang_property <- function(lang_val) {
  if (is.null(lang_val)) return(TRUE)
  if (!is.character(lang_val) || length(lang_val) != 1) {
    stop("lang property must be a single string")
  }
  if (!validate_bcp47(lang_val)) {
    stop(paste("Invalid BCP 47 language tag:", lang_val))
  }
  return(TRUE)
}

validate_separator_property <- function(sep) {
  if (is.null(sep)) return(TRUE)
  if (!is.character(sep) || length(sep) != 1) {
    stop("separator must be a single string")
  }
  return(TRUE)
}

validate_ordered_property <- function(ord) {
  if (is.null(ord)) return(TRUE)
  validate_boolean(ord, "ordered")
}

validate_trim_property <- function(tr) {
  if (is.null(tr)) return(TRUE)
  if (is.logical(tr)) return(TRUE)
  if (is.character(tr) && length(tr) == 1) {
    if (tr %in% c("true", "false", "start", "end")) return(TRUE)
  }
  stop("trim must be a boolean or one of 'start', 'end'")
}

validate_textDirection_property <- function(td) {
  if (is.null(td)) return(TRUE)
  if (is.character(td) && length(td) == 1) {
    if (td %in% c("ltr", "rtl", "auto")) return(TRUE)
  }
  stop("textDirection must be one of 'ltr', 'rtl', 'auto'")
}

validate_tableDirection_property <- function(td) {
  if (is.null(td)) return(TRUE)
  if (is.character(td) && length(td) == 1) {
    if (td %in% c("ltr", "rtl", "auto")) return(TRUE)
  }
  stop("tableDirection must be one of 'ltr', 'rtl', 'auto'")
}

validate_uri_template_property <- function(val, name) {
  if (is.null(val)) return(TRUE)
  if (!is.character(val) || length(val) != 1) {
    stop(sprintf("%s property must be a single string representing a URI template", name))
  }
  return(TRUE)
}

validate_object_properties <- function(obj, allowed_properties, is_description = TRUE, name = "object") {
  if (is.null(obj) || !is.list(obj)) return(obj)
  
  BUILT_IN_PREFIXES <- c("grddl", "ma", "owl", "rdf", "rdfa", "rdfs", "rif", "skos", "xml", "xsd", 
                         "cc", "ctag", "dc", "dcterms", "foaf", "gr", "ical", "og", "rev", "sioc", 
                         "v", "vcard", "schema", "wdr", "wdrs", "xhv", "csvw", "dcat", "prov")
  
  keys <- names(obj)
  filtered_obj <- obj
  for (k in keys) {
    if (k == "") next
    if (k %in% allowed_properties) next
    
    if (startsWith(k, "@")) {
      if (k %in% c("@id", "@type", "@context", "@language", "@base")) next
      warning(sprintf("Invalid keyword '%s' in %s", k, name))
      filtered_obj[[k]] <- NULL
      next
    }
    
    if (is_description) {
      if (grepl("^[a-zA-Z_][a-zA-Z0-9_-]*:", k)) {
        prefix <- strsplit(k, ":")[[1]][1]
        if (prefix %in% c("http", "https") || prefix %in% BUILT_IN_PREFIXES) {
          next
        }
      }
    }
    
    warning(sprintf("Property '%s' is not allowed on %s", k, name))
    filtered_obj[[k]] <- NULL
  }
  return(filtered_obj)
}

extract_context_properties <- function(md) {
  context <- md[["@context"]]
  base_val <- NULL
  lang_val <- NULL
  
  if (is.list(context)) {
    if (is.null(names(context))) {
      for (item in context) {
        if (is.list(item) && !is.null(names(item))) {
          if (!is.null(item[["@base"]])) base_val <- item[["@base"]]
          if (!is.null(item[["@language"]])) lang_val <- item[["@language"]]
        }
      }
    } else {
      if (!is.null(context[["@base"]])) base_val <- context[["@base"]]
      if (!is.null(context[["@language"]])) lang_val <- context[["@language"]]
    }
  }
  
  list(base = base_val, lang = lang_val)
}

validate_context <- function(context) {
  if (is.null(context)) return(TRUE)
  if (is.character(context)) {
    if (length(context) == 1 && context != "http://www.w3.org/ns/csvw") {
      stop("Invalid @context URI")
    }
    return(TRUE)
  }
  if (is.list(context)) {
    if (is.null(names(context))) {
      if (length(context) < 1 || context[[1]] != "http://www.w3.org/ns/csvw") {
        stop("First element of @context must be http://www.w3.org/ns/csvw")
      }
      if (length(context) > 1) {
        extra <- context[[2]]
        if (!is.list(extra) || is.null(names(extra))) {
          stop("Second element of @context must be an object")
        }
        keys <- names(extra)
        for (k in keys) {
          if (!k %in% c("@base", "@language")) {
            stop(paste("Invalid property in @context:", k))
          }
        }
      }
    } else {
      stop("Invalid @context format")
    }
  }
  return(TRUE)
}

validate_jsonld_value <- function(x, allow_context = FALSE) {
  if (is.null(x)) return(TRUE)
  
  if (is.list(x)) {
    # Check if it has keys/names
    keys <- names(x)
    if (!is.null(keys) && !all(keys == "")) {
      # It is a key-value object
      for (k in keys) {
        if (startsWith(k, "@")) {
          allowed <- c("@id", "@type", "@value", "@language", "@direction")
          if (allow_context) {
            allowed <- c(allowed, "@context", "@base")
          }
          if (!(k %in% allowed)) {
            stop(paste("Invalid JSON-LD keyword:", k))
          }
        }
      }
      
      # If @value is present
      if ("@value" %in% keys) {
        # Check extra keys
        extra_keys <- setdiff(keys, c("@value", "@type", "@language", "@direction"))
        if (length(extra_keys) > 0) {
          stop(paste("Value object has extra properties:", paste(extra_keys, collapse = ", ")))
        }
        
        # Check type
        if ("@type" %in% keys) {
          t_val <- x[["@type"]]
          if (!is.character(t_val) || length(t_val) != 1) {
            stop("@type in value object must be a single string")
          }
          if (startsWith(t_val, "_:")) {
            stop("Blank node identifier is not allowed as @type")
          }
          if (grepl("\\s", t_val)) {
            stop("Invalid character in @type value")
          }
          if ("@language" %in% keys || "@direction" %in% keys) {
            stop("Value object cannot have both @type and @language/@direction")
          }
        }
        
        # Check language
        if ("@language" %in% keys) {
          l_val <- x[["@language"]]
          if (!is.character(l_val) || length(l_val) != 1) {
            stop("@language must be a single string")
          }
          if (!validate_bcp47(l_val)) {
            stop(paste("Invalid BCP 47 language tag in @language:", l_val))
          }
          val <- x[["@value"]]
          if (!is.character(val) || length(val) != 1) {
            stop("@value must be a string when @language is present")
          }
        }
        
        # Check direction
        if ("@direction" %in% keys) {
          d_val <- x[["@direction"]]
          if (!is.character(d_val) || length(d_val) != 1 || !(d_val %in% c("ltr", "rtl"))) {
            stop("@direction must be ltr or rtl")
          }
          val <- x[["@value"]]
          if (!is.character(val) || length(val) != 1) {
            stop("@value must be a string when @direction is present")
          }
        }
      } else {
        # @value is NOT present
        if (!allow_context) {
          if ("@language" %in% keys || "@direction" %in% keys) {
            stop("@language and @direction are only allowed inside a value object")
          }
        }
        if ("@type" %in% keys) {
          t_val <- x[["@type"]]
          if (!is.character(t_val) || length(t_val) == 0) {
            stop("@type must be a string or array of strings")
          }
          for (t_item in t_val) {
            if (startsWith(t_item, "_:")) {
              stop("Blank node identifier is not allowed as @type")
            }
            if (grepl("\\s", t_item)) {
              stop("Invalid character in @type value")
            }
          }
        }
        if ("@id" %in% keys) {
          id_val <- x[["@id"]]
          if (!is.character(id_val) || length(id_val) != 1) {
            stop("@id must be a single string")
          }
          if (startsWith(id_val, "_:")) {
            stop("Blank node identifier is not allowed as @id")
          }
        }
      }
      
      # Recursively validate values
      for (k in keys) {
        if (k != "@context") {
          validate_jsonld_value(x[[k]], allow_context = FALSE)
        }
      }
    } else {
      # Array of unnamed values
      for (item in x) {
        validate_jsonld_value(item, allow_context = FALSE)
      }
    }
  }
  return(TRUE)
}

#' Parse Column list structure
#' @param c list representing column
#' @param parent parent Table object
#' @param generated logical, if TRUE represents an autogenerated column
#' @return a csvw_column object
parse_column <- function(c, parent = NULL, generated = FALSE) {
  if (is.null(c)) return(NULL)
  if (!is.list(c)) {
    stop("Column description must be an object")
  }
  
  c <- validate_object_properties(c, c("@id", "@type", "name", "titles", "required", "virtual", "suppressOutput", "aboutUrl", "propertyUrl", "valueUrl", "datatype", "null", "separator", "trim", "ordered", "lang", "default", "textDirection"), name = "Column")
  c[["null"]] <- check_prop(c[["null"]], validate_null_property)
  c[["lang"]] <- check_prop(c[["lang"]], validate_lang_property)
  c[["separator"]] <- check_prop(c[["separator"]], validate_separator_property)
  c[["ordered"]] <- check_prop(c[["ordered"]], validate_ordered_property)
  c[["trim"]] <- check_prop(c[["trim"]], validate_trim_property)
  c[["textDirection"]] <- check_prop(c[["textDirection"]], validate_textDirection_property)
  
  c[["aboutUrl"]] <- check_prop(c[["aboutUrl"]], function(x) validate_uri_template_property(x, "aboutUrl"))
  c[["propertyUrl"]] <- check_prop(c[["propertyUrl"]], function(x) validate_uri_template_property(x, "propertyUrl"))
  c[["valueUrl"]] <- check_prop(c[["valueUrl"]], function(x) validate_uri_template_property(x, "valueUrl"))
  c[["default"]] <- check_prop(c[["default"]], function(x) {
    if (!is.character(x)) stop("default property must be a string")
  })
  
  check_id(c[["@id"]])
  check_type(c[["@type"]], "Column")
  c[["titles"]] <- check_prop(c[["titles"]], validate_titles)
  c[["required"]] <- check_prop(c[["required"]], function(x) validate_boolean(x, "required"))
  c[["virtual"]] <- check_prop(c[["virtual"]], function(x) validate_boolean(x, "virtual"))
  c[["suppressOutput"]] <- check_prop(c[["suppressOutput"]], function(x) validate_boolean(x, "suppressOutput"))
  
  name_val <- c[["name"]]
  if (!is.null(name_val)) {
    valid_name <- tryCatch({
      if (!is.character(name_val) || length(name_val) != 1) {
        stop("Column name must be a single string")
      }
      if (!generated) {
        if (startsWith(name_val, "_")) {
          stop("Column name must not start with _")
        }
        if (!grepl("^[a-zA-Z0-9%_]+(\\.[a-zA-Z0-9%_]+)*$", name_val)) {
          stop("Column name contains invalid characters")
        }
      }
      TRUE
    }, error = function(e) {
      warning(e$message)
      FALSE
    })
    if (!valid_name) {
      c[["name"]] <- NULL
      name_val <- NULL
    }
  }
  
  if (is.null(name_val) || identical(name_val, "") || identical(name_val, list())) {
    name_val <- get_first_title(c[["titles"]])
  }
  if (is.null(name_val) || identical(name_val, "") || identical(name_val, list())) {
    name_val <- ""
  }
  
  col <- list(
    name = name_val,
    titles = c[["titles"]],
    has_name = !is.null(c[["name"]]),
    has_titles = !is.null(c[["titles"]]),
    datatype = parse_datatype(c[["datatype"]]),
    separator = c[["separator"]],
    null = if (!is.null(c[["null"]])) c[["null"]] else "",
    required = if (!is.null(c[["required"]])) as.logical(c[["required"]]) else FALSE,
    virtual = if (!is.null(c[["virtual"]])) as.logical(c[["virtual"]]) else FALSE,
    suppressOutput = if (!is.null(c[["suppressOutput"]])) as.logical(c[["suppressOutput"]]) else FALSE,
    propertyUrl = c[["propertyUrl"]],
    valueUrl = c[["valueUrl"]],
    aboutUrl = c[["aboutUrl"]],
    default = if (!is.null(c[["default"]])) c[["default"]] else "",
    parent = parent
  )
  class(col) <- "csvw_column"
  col
}

parse_table_schema <- function(s, parent = NULL, base_path = "") {
  if (is.null(s)) return(NULL)
  
  schema_url <- NULL
  if (is.character(s) && length(s) == 1) {
    resolved_schema_url <- resolve_url(base_path, s)
    schema_url <- resolved_schema_url
    s <- get_json(resolved_schema_url)
  }
  
  if (!is.list(s)) {
    stop("tableSchema must be an object or a URI string")
  }
  
  s <- validate_object_properties(s, c("@id", "@type", "columns", "primaryKey", "foreignKeys", "rowTitles", "aboutUrl", "propertyUrl", "valueUrl", "datatype", "null", "separator", "trim", "ordered", "lang", "textDirection", "default", "required"), name = "Schema")
  s[["null"]] <- check_prop(s[["null"]], validate_null_property)
  s[["lang"]] <- check_prop(s[["lang"]], validate_lang_property)
  s[["separator"]] <- check_prop(s[["separator"]], validate_separator_property)
  s[["ordered"]] <- check_prop(s[["ordered"]], validate_ordered_property)
  s[["trim"]] <- check_prop(s[["trim"]], validate_trim_property)
  s[["textDirection"]] <- check_prop(s[["textDirection"]], validate_textDirection_property)
  
  s[["aboutUrl"]] <- check_prop(s[["aboutUrl"]], function(x) validate_uri_template_property(x, "aboutUrl"))
  s[["propertyUrl"]] <- check_prop(s[["propertyUrl"]], function(x) validate_uri_template_property(x, "propertyUrl"))
  s[["valueUrl"]] <- check_prop(s[["valueUrl"]], function(x) validate_uri_template_property(x, "valueUrl"))
  s[["default"]] <- check_prop(s[["default"]], function(x) {
    if (!is.character(x)) stop("default property must be a string")
  })
  
  check_id(s[["@id"]])
  check_type(s[["@type"]], "Schema")
  s[["rowTitles"]] <- check_prop(s[["rowTitles"]], validate_titles)
  
  raw_cols <- s[["columns"]]
  columns <- list()
  if (!is.null(raw_cols)) {
    for (j in seq_along(raw_cols)) {
      col <- parse_column(raw_cols[[j]], parent = parent)
      if (is.null(col$name) || col$name == "") {
        col$name <- paste0("_col.", j)
      }
      columns[[j]] <- col
    }
  }
  
  col_names <- sapply(columns, function(c) c$name)
  if (any(duplicated(col_names))) {
    stop("Duplicate column names are not allowed")
  }
  
  seen_virtual <- FALSE
  for (col in columns) {
    if (col$virtual) {
      seen_virtual <- TRUE
    } else {
      if (seen_virtual) {
        stop("Virtual columns must appear after all non-virtual columns")
      }
    }
  }
  
  pk <- s[["primaryKey"]]
  if (!is.null(pk)) {
    pk_cols <- unlist(pk)
    for (pk_col_name in pk_cols) {
      col_idx <- which(col_names == pk_col_name)
      if (length(col_idx) == 0) {
        stop("primaryKey must refer to existing columns")
      }
      col <- columns[[col_idx[1]]]
      if (!col$has_name) {
        stop(sprintf("Referenced primaryKey column '%s' must have an explicit name property", pk_col_name))
      }
    }
  }
  
  fks <- s[["foreignKeys"]]
  if (!is.null(fks)) {
    if (!is.list(fks)) stop("foreignKeys must be an array")
    for (fk in fks) {
      if (!is.list(fk)) stop("Each foreignKey must be an object")
      validate_object_properties(fk, c("columnReference", "reference"), is_description = FALSE, name = "foreignKey")
      
      if (is.null(fk[["columnReference"]])) stop("foreignKey must have columnReference")
      ref_cols <- unlist(fk[["columnReference"]])
      for (ref_col_name in ref_cols) {
        col_idx <- which(col_names == ref_col_name)
        if (length(col_idx) == 0) {
          stop("foreignKey columnReference must refer to existing columns")
        }
        col <- columns[[col_idx[1]]]
        if (!col$has_name) {
          stop(sprintf("Referenced foreignKey columnReference '%s' must have an explicit name property", ref_col_name))
        }
      }
      
      ref <- fk[["reference"]]
      if (is.null(ref) || !is.list(ref)) stop("foreignKey must have a reference object")
      validate_object_properties(ref, c("resource", "schema", "schemaReference", "columnReference"), is_description = FALSE, name = "foreignKey reference")
      
      if (is.null(ref[["columnReference"]])) stop("foreignKey reference must have columnReference")
      if (is.null(ref[["resource"]]) && is.null(ref[["schema"]]) && is.null(ref[["schemaReference"]])) {
        stop("foreignKey reference must have resource, schema, or schemaReference")
      }
    }
  }
  
  schema <- list(
    id = s[["@id"]],
    schema_url = schema_url,
    columns = columns,
    primaryKey = s[["primaryKey"]],
    foreignKeys = s[["foreignKeys"]],
    rowTitles = s[["rowTitles"]],
    aboutUrl = s[["aboutUrl"]],
    datatype = if (!is.null(s[["datatype"]])) parse_datatype(s[["datatype"]]) else NULL,
    null = s[["null"]],
    separator = s[["separator"]],
    trim = s[["trim"]],
    propertyUrl = s[["propertyUrl"]],
    valueUrl = s[["valueUrl"]],
    required = s[["required"]],
    ordered = s[["ordered"]],
    lang = s[["lang"]],
    textDirection = s[["textDirection"]],
    default = s[["default"]]
  )
  class(schema) <- "csvw_table_schema"
  schema
}

#' Parse Table list structure
#' @param t list representing table
#' @param parent parent TableGroup object
#' @param base_path character directory path or base URL
#' @return a csvw_table object
parse_table <- function(t, parent = NULL, base_path = "") {
  if (is.null(t)) return(NULL)
  
  t <- validate_object_properties(t, c("@id", "@type", "url", "dialect", "notes", "tableSchema", "tableDirection", "templates", "translations", "null", "separator", "trim", "aboutUrl", "propertyUrl", "valueUrl", "datatype", "required", "ordered", "lang", "suppressOutput", "textDirection", "default"), name = "Table")
  t[["null"]] <- check_prop(t[["null"]], validate_null_property)
  t[["lang"]] <- check_prop(t[["lang"]], validate_lang_property)
  t[["separator"]] <- check_prop(t[["separator"]], validate_separator_property)
  t[["ordered"]] <- check_prop(t[["ordered"]], validate_ordered_property)
  t[["trim"]] <- check_prop(t[["trim"]], validate_trim_property)
  t[["textDirection"]] <- check_prop(t[["textDirection"]], validate_textDirection_property)
  t[["tableDirection"]] <- check_prop(t[["tableDirection"]], validate_tableDirection_property)
  
  templates <- t[["templates"]]
  if (!is.null(templates)) {
    valid_templates <- tryCatch({
      if (!is.list(templates)) stop("templates must be an array")
      for (i in seq_along(templates)) {
        trans <- templates[[i]]
        if (!is.list(trans)) stop("transformation must be an object")
        trans <- validate_object_properties(trans, c("url", "titles", "scriptFormat", "targetFormat", "source"), is_description = TRUE, name = "transformation")
        templates[[i]] <- trans
      }
      list(ok = TRUE, val = templates)
    }, error = function(e) {
      warning(e$message)
      list(ok = FALSE)
    })
    if (valid_templates$ok) {
      t[["templates"]] <- valid_templates$val
    } else {
      t[["templates"]] <- NULL
    }
  }
  
  check_id(t[["@id"]])
  check_type(t[["@type"]], "Table")
  t[["titles"]] <- check_prop(t[["titles"]], validate_titles)
  t[["suppressOutput"]] <- check_prop(t[["suppressOutput"]], function(x) validate_boolean(x, "suppressOutput"))
  
  raw_url <- t[["url"]]
  if (is.null(raw_url) || identical(raw_url, "")) {
    stop("Table must have a url property")
  }
  
  resolved_url <- resolve_url(base_path, raw_url)
  
  if (!is.null(t[["tableSchema"]]) && !is.list(t[["tableSchema"]]) && !is.character(t[["tableSchema"]])) {
    stop("tableSchema must be an object or a URI string")
  }
  tableSchema <- parse_table_schema(t[["tableSchema"]], parent = parent, base_path = base_path)
  
  if (!is.null(t[["dialect"]]) && !is.list(t[["dialect"]]) && !is.character(t[["dialect"]])) {
    stop("dialect must be an object or a URI string")
  }
  parent_dialect <- if (!is.null(parent)) parent$dialect else NULL
  dialect <- parse_dialect(t[["dialect"]], fallback = parent_dialect, base_path = base_path)
  
  # Validate URI template properties
  t[["aboutUrl"]] <- check_prop(t[["aboutUrl"]], function(x) validate_uri_template_property(x, "aboutUrl"))
  t[["propertyUrl"]] <- check_prop(t[["propertyUrl"]], function(x) validate_uri_template_property(x, "propertyUrl"))
  t[["valueUrl"]] <- check_prop(t[["valueUrl"]], function(x) validate_uri_template_property(x, "valueUrl"))
  t[["default"]] <- check_prop(t[["default"]], function(x) {
    if (!is.character(x)) stop("default property must be a string")
  })

  table <- list(
    url = resolved_url,
    tableSchema = tableSchema,
    dialect = dialect,
    id = t[["@id"]],
    suppressOutput = if (!is.null(t[["suppressOutput"]])) as.logical(t[["suppressOutput"]]) else FALSE,
    notes = t[["notes"]],
    null = t[["null"]],
    separator = t[["separator"]],
    trim = t[["trim"]],
    aboutUrl = t[["aboutUrl"]],
    propertyUrl = t[["propertyUrl"]],
    valueUrl = t[["valueUrl"]],
    datatype = if (!is.null(t[["datatype"]])) parse_datatype(t[["datatype"]]) else NULL,
    required = t[["required"]],
    ordered = t[["ordered"]],
    lang = t[["lang"]],
    textDirection = t[["textDirection"]],
    default = t[["default"]],
    common_properties = t[setdiff(names(t), c("url", "tableSchema", "dialect", "suppressOutput", "notes", "null", "separator", "trim", "aboutUrl", "propertyUrl", "valueUrl", "datatype", "required", "ordered", "lang", "textDirection", "default", "@context", "@base", "@id", "@type"))],
    parent = parent,
    data = NULL
  )
  class(table) <- "csvw_table"
  
  if (!is.null(table$tableSchema)) {
    table$tableSchema$columns <- lapply(table$tableSchema$columns, function(col) {
      col$parent <- table
      col
    })
  }
  
  table
}

parse_table_group <- function(tg, base_path = "") {
  if (is.null(tg)) return(NULL)
  
  tg <- validate_object_properties(tg, c("@id", "@type", "tables", "dialect", "notes", "tableSchema", "tableDirection", "templates", "translations", "null", "separator", "trim", "aboutUrl", "propertyUrl", "valueUrl", "datatype", "required", "ordered", "lang", "textDirection", "default"), name = "TableGroup")
  tg[["null"]] <- check_prop(tg[["null"]], validate_null_property)
  tg[["lang"]] <- check_prop(tg[["lang"]], validate_lang_property)
  tg[["separator"]] <- check_prop(tg[["separator"]], validate_separator_property)
  tg[["ordered"]] <- check_prop(tg[["ordered"]], validate_ordered_property)
  tg[["trim"]] <- check_prop(tg[["trim"]], validate_trim_property)
  tg[["textDirection"]] <- check_prop(tg[["textDirection"]], validate_textDirection_property)
  tg[["tableDirection"]] <- check_prop(tg[["tableDirection"]], validate_tableDirection_property)
  
  templates <- tg[["templates"]]
  if (!is.null(templates)) {
    valid_templates <- tryCatch({
      if (!is.list(templates)) stop("templates must be an array")
      for (i in seq_along(templates)) {
        trans <- templates[[i]]
        if (!is.list(trans)) stop("transformation must be an object")
        trans <- validate_object_properties(trans, c("url", "titles", "scriptFormat", "targetFormat", "source"), is_description = TRUE, name = "transformation")
        templates[[i]] <- trans
      }
      list(ok = TRUE, val = templates)
    }, error = function(e) {
      warning(e$message)
      list(ok = FALSE)
    })
    if (valid_templates$ok) {
      tg[["templates"]] <- valid_templates$val
    } else {
      tg[["templates"]] <- NULL
    }
  }
  
  check_id(tg[["@id"]])
  check_type(tg[["@type"]], "TableGroup")
  tg[["titles"]] <- check_prop(tg[["titles"]], validate_titles)
  
  if (!is.null(tg[["dialect"]]) && !is.list(tg[["dialect"]]) && !is.character(tg[["dialect"]])) {
    stop("dialect must be an object or a URI string")
  }
  dialect <- parse_dialect(tg[["dialect"]], base_path = base_path)
  
  raw_tables <- tg[["tables"]]
  if (is.null(raw_tables) || length(raw_tables) == 0) {
    stop("TableGroup tables must contain at least one table")
  }
  
  # Validate URI template properties
  tg[["aboutUrl"]] <- check_prop(tg[["aboutUrl"]], function(x) validate_uri_template_property(x, "aboutUrl"))
  tg[["propertyUrl"]] <- check_prop(tg[["propertyUrl"]], function(x) validate_uri_template_property(x, "propertyUrl"))
  tg[["valueUrl"]] <- check_prop(tg[["valueUrl"]], function(x) validate_uri_template_property(x, "valueUrl"))
  
  if (!is.null(tg[["default"]]) && !is.character(tg[["default"]])) {
    stop("default property must be a string")
  }

  group <- list(
    tables = list(),
    id = tg[["@id"]],
    dialect = dialect,
    notes = tg[["notes"]],
    null = tg[["null"]],
    separator = tg[["separator"]],
    trim = tg[["trim"]],
    aboutUrl = tg[["aboutUrl"]],
    propertyUrl = tg[["propertyUrl"]],
    valueUrl = tg[["valueUrl"]],
    datatype = if (!is.null(tg[["datatype"]])) parse_datatype(tg[["datatype"]]) else NULL,
    required = tg[["required"]],
    ordered = tg[["ordered"]],
    lang = tg[["lang"]],
    textDirection = tg[["textDirection"]],
    default = tg[["default"]],
    common_properties = tg[setdiff(names(tg), c("tables", "dialect", "notes", "null", "separator", "trim", "aboutUrl", "propertyUrl", "valueUrl", "datatype", "required", "ordered", "lang", "textDirection", "default", "@context", "@base", "@id", "@type"))]
  )
  class(group) <- "csvw_table_group"
  
  tables <- lapply(raw_tables, parse_table, parent = group, base_path = base_path)
  group$tables <- tables
  
  group
}

#' Helper to inherit value from parent structure
#' @param col column object
#' @param name property name
#' @return value or default
inherit_val <- function(col, name) {
  if (!is.null(col[[name]]) && !identical(col[[name]], "")) return(col[[name]])
  tbl <- col$parent
  if (!is.null(tbl)) {
    sch <- tbl$tableSchema
    if (!is.null(sch) && !is.null(sch[[name]]) && !identical(sch[[name]], "")) return(sch[[name]])
    if (!is.null(tbl[[name]]) && !identical(tbl[[name]], "")) return(tbl[[name]])
    grp <- tbl$parent
    if (!is.null(grp) && !is.null(grp[[name]]) && !identical(grp[[name]], "")) return(grp[[name]])
  }
  
  if (name == "null") return("")
  if (name == "required") return(FALSE)
  if (name == "ordered") return(FALSE)
  return(NULL)
}

#' Helper to inherit null values
#' @param col column object
#' @return character vector of null representations
inherit_null <- function(col) {
  if (identical(col$null, "") || identical(col$null, list(""))) {
    tbl <- col$parent
    if (!is.null(tbl)) {
      sch <- tbl$tableSchema
      if (!is.null(sch) && !is.null(sch$null) && !identical(sch$null, "") && !identical(sch$null, list(""))) {
        return(sch$null)
      }
      tbl_null <- tbl$null
      if (!is.null(tbl_null) && !identical(tbl_null, "") && !identical(tbl_null, list(""))) {
        return(tbl_null)
      }
      grp <- tbl$parent
      if (!is.null(grp)) {
        grp_null <- grp$null
        if (!is.null(grp_null) && !identical(grp_null, "") && !identical(grp_null, list(""))) {
          return(grp_null)
        }
      }
    }
  }
  return(col$null)
}

trim_value <- function(val_str, trim_opt, skip_initial_space) {
  if (is.na(val_str) || is.null(val_str)) return(val_str)
  
  if (identical(skip_initial_space, TRUE) || identical(skip_initial_space, "true")) {
    val_str <- sub("^\\s+", "", val_str)
  }
  
  if (identical(trim_opt, TRUE) || identical(trim_opt, "true") || identical(trim_opt, "start")) {
    val_str <- sub("^\\s+", "", val_str)
  }
  if (identical(trim_opt, TRUE) || identical(trim_opt, "true") || identical(trim_opt, "end")) {
    val_str <- sub("\\s+$", "", val_str)
  }
  return(val_str)
}

#' Check if metadata describes the CSV file
#' @param md list representing parsed metadata
#' @param url path or URL of CSV file
#' @return logical
does_metadata_describe_file <- function(md, url) {
  if (is.null(md)) return(FALSE)
  
  abs_csv_url <- normalize_url_local(url)
  
  meta_base_url <- if (is_url(url)) url else dirname(normalizePath(url, mustWork = FALSE))
  base_path <- meta_base_url
  if (!is.null(md[["@context"]])) {
    ctx_props <- extract_context_properties(md)
    if (!is.null(ctx_props$base)) {
      base_path <- resolve_url(meta_base_url, ctx_props$base)
    }
  }
  if (!is.null(md[["@base"]])) {
    base_path <- resolve_url(meta_base_url, md[["@base"]])
  }
  
  if (!is.null(md[["tables"]])) {
    for (tbl in md[["tables"]]) {
      tbl_url <- tbl[["url"]]
      if (!is.null(tbl_url)) {
        resolved_tbl_url <- resolve_url(base_path, tbl_url)
        if (normalize_url_local(resolved_tbl_url) == abs_csv_url) {
          return(TRUE)
        }
      }
    }
  } else {
    tbl_url <- md[["url"]]
    if (!is.null(tbl_url)) {
      resolved_tbl_url <- resolve_url(base_path, tbl_url)
      if (normalize_url_local(resolved_tbl_url) == abs_csv_url) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

#' Locate metadata matching CSVW specifications
#' @param url path or URL of CSV file
#' @param md_url optional explicit metadata path or URL
#' @return list with parsed JSON md structure and no_header logical
locate_metadata <- function(url, md_url = NULL) {
  if (!is.null(md_url)) {
    return(list(md = get_json(md_url), no_header = FALSE, is_located = FALSE))
  }
  
  # Try loading URL directly as JSON
  md <- tryCatch({
    get_json(url)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(md)) {
    return(list(md = md, no_header = FALSE, is_located = FALSE))
  }
  
  no_header <- FALSE
  
  if (is_url(url)) {
    # 1. Check Link Header (describedby)
    resp <- tryCatch({
      httr::HEAD(url)
    }, error = function(e) NULL)
    
    if (!is.null(resp)) {
      content_type <- httr::headers(resp)[["content-type"]]
      if (!is.null(content_type) && grepl("header\\s*=\\s*absent", content_type)) {
        no_header <- TRUE
      }
      link_hdr <- httr::headers(resp)[["link"]]
      if (!is.null(link_hdr)) {
        m <- regexec("<([^>]+)>;\\s*rel=\"describedby\"", link_hdr)
        if (m[[1]][1] != -1) {
          md_rel_url <- regmatches(link_hdr, m)[[1]][2]
          md_full_url <- resolve_url(url, md_rel_url)
          md <- tryCatch({ get_json(md_full_url) }, error = function(e) NULL)
          if (!is.null(md) && does_metadata_describe_file(md, url)) {
            return(list(md = md, no_header = no_header, is_located = TRUE))
          } else if (!is.null(md)) {
            warning(paste("Link header metadata does not describe the CSV file:", url))
          }
        }
      }
    }
    
    # 2. Check site-wide /.well-known/csvm configuration
    well_known_url <- resolve_url(url, "/.well-known/csvm")
    well_known <- tryCatch({
      resp <- httr::GET(well_known_url)
      if (httr::status_code(resp) == 200) httr::content(resp, as = "text") else NULL
    }, error = function(e) NULL)
    
    locs <- if (!is.null(well_known)) strsplit(well_known, "\n")[[1]] else c("{+url}-metadata.json", "csv-metadata.json")
    for (line in locs) {
      line <- trimws(line)
      if (line == "" || startsWith(line, "#")) next
      expanded_line <- expand_uri_template(line, list(url = url))
      md_url_try <- resolve_url(url, expanded_line)
      md <- tryCatch({ get_json(md_url_try) }, error = function(e) NULL)
      if (!is.null(md) && does_metadata_describe_file(md, url)) {
        return(list(md = md, no_header = no_header, is_located = TRUE))
      } else if (!is.null(md)) {
        warning(paste("Site-wide metadata does not describe the CSV file:", url))
      }
    }
  } else {
    # Local file metadata locations
    file_md_path <- paste0(url, "-metadata.json")
    if (file.exists(file_md_path)) {
      md <- get_json(file_md_path)
      if (does_metadata_describe_file(md, url)) {
        return(list(md = md, no_header = no_header, is_located = TRUE))
      } else {
        warning(paste("File metadata does not describe the CSV file:", url))
      }
    }
    dir_md_path <- file.path(dirname(url), "metadata.json")
    if (file.exists(dir_md_path)) {
      md <- get_json(dir_md_path)
      if (does_metadata_describe_file(md, url)) {
        return(list(md = md, no_header = no_header, is_located = TRUE))
      } else {
        warning(paste("Directory metadata does not describe the CSV file:", url))
      }
    }
  }
  
  # Fallback: metadata-free description
  base_val <- if (is_url(url)) url else dirname(normalizePath(url, mustWork = FALSE))
  url_val <- if (is_url(url)) url else basename(url)
  
  md <- list(
    `@context` = "http://www.w3.org/ns/csvw",
    `@base` = base_val,
    url = url_val
  )
  return(list(md = md, no_header = no_header, is_located = FALSE))
}

#' Read data for a CSVW table
#' @param table csvw_table object
#' @param strict logical, if TRUE throws on validation errors
#' @param validate logical, if TRUE performs validation checks during reading
#' @param lax logical, if TRUE ignores excess columns / compatibility issues during lax validation
#' @return list of row lists
read_table_csv <- function(table, strict = TRUE, validate = FALSE, lax = FALSE) {
  dialect <- table$dialect
  if (is.null(dialect)) dialect <- csvw_dialect()
  
  file_url <- normalize_url_local(table$url)
  temp_dir <- NULL
  if (!file.exists(file_url) && file.exists(paste0(file_url, ".zip"))) {
    zip_file <- paste0(file_url, ".zip")
    temp_dir <- tempfile("csvw_zip")
    dir.create(temp_dir, showWarnings = FALSE)
    extracted_files <- unzip(zip_file, exdir = temp_dir)
    if (length(extracted_files) > 0) {
      base_name <- basename(file_url)
      match_idx <- which(basename(extracted_files) == base_name)
      if (length(match_idx) > 0) {
        file_url <- extracted_files[match_idx[1]]
      } else {
        file_url <- extracted_files[1]
      }
    }
  }
  if (!is.null(temp_dir)) {
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  }
  
  loc <- readr::locale(encoding = dialect$encoding)
  escape_double <- dialect$doubleQuote
  skip <- dialect$skipRows
  
  quote <- dialect$quoteChar
  if (is.null(quote)) quote <- "\""
  delim <- dialect$delimiter
  if (is.null(delim)) delim <- ","
  
  raw_df <- tryCatch({
    suppressMessages(
      readr::read_delim(
        file = file_url,
        delim = delim,
        quote = quote,
        escape_double = escape_double,
        col_names = FALSE,
        col_types = readr::cols(.default = readr::col_character()),
        skip = skip,
        locale = loc,
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }, error = function(e) {
    stop(paste("Failed to read CSV file:", file_url, "-", e$message))
  })
  
  if (nrow(raw_df) == 0) return(list())
  
  # Process skipColumns
  if (dialect$skipColumns > 0 && ncol(raw_df) > dialect$skipColumns) {
    raw_df <- raw_df[, (dialect$skipColumns + 1):ncol(raw_df), drop = FALSE]
  }
  
  # Resolve header row
  header_names <- NULL
  if (dialect$header) {
    header_count <- dialect$headerRowCount
    if (header_count < 1) header_count <- 1
    header_rows <- raw_df[1:header_count, , drop = FALSE]
    header_names <- as.character(unlist(header_rows[header_count, ]))
    header_names <- sapply(header_names, function(h) {
      trim_value(h, dialect$trim, dialect$skipInitialSpace)
    })
    raw_df <- raw_df[(header_count + 1):nrow(raw_df), , drop = FALSE]
  } else {
    header_names <- paste0("_col.", 1:ncol(raw_df))
  }
  
  # Remove skipped blank rows
  if (dialect$skipBlankRows) {
    is_blank <- apply(raw_df, 1, function(row) all(is.na(row) | row == "" | trimws(row) == ""))
    raw_df <- raw_df[!is_blank, , drop = FALSE]
  }
  
  if (nrow(raw_df) == 0) return(list())
  
  schema <- table$tableSchema
  schema_columns <- if (!is.null(schema)) schema$columns else list()
  
  regular_cols <- list()
  virtual_cols <- list()
  for (col in schema_columns) {
    if (col$virtual) {
      virtual_cols[[length(virtual_cols) + 1]] <- col
    } else {
      regular_cols[[length(regular_cols) + 1]] <- col
    }
  }
  
  # Check number of columns
  if (!is.null(schema)) {
    if (ncol(raw_df) != length(regular_cols)) {
      if (validate && !lax) {
        stop(sprintf("Number of columns in CSV (%d) does not match number of non-virtual columns in schema (%d)", ncol(raw_df), length(regular_cols)))
      }
    }
  }

  # Default column maps
  if (length(regular_cols) == 0) {
    regular_cols <- lapply(header_names, function(name) {
      parse_column(list(name = name), generated = TRUE)
    })
  }
  
  languages_match <- function(lang1, lang2) {
    if (is.null(lang1) || is.null(lang2)) return(TRUE)
    if (lang1 == "und" || lang2 == "und") return(TRUE)
    parts1 <- strsplit(tolower(lang1), "-", fixed = TRUE)[[1]]
    parts2 <- strsplit(tolower(lang2), "-", fixed = TRUE)[[1]]
    min_len <- min(length(parts1), length(parts2))
    for (k in 1:min_len) {
      if (parts1[k] != parts2[k]) return(FALSE)
    }
    return(TRUE)
  }
  
  table_lang <- if (!is.null(table$lang)) table$lang else "und"
  
  matched_cols <- list()
  for (j in seq_along(header_names)) {
    hname <- header_names[j]
    col <- NULL
    if (j <= length(regular_cols)) {
      col <- regular_cols[[j]]
    } else {
      col <- parse_column(list(name = paste0("_col.", j)), generated = TRUE)
    }
    
    # Check compatibility if dialect$header is TRUE and schema columns are present
    if (dialect$header && !is.null(schema) && j <= length(regular_cols)) {
      if (col$has_titles || col$has_name) {
        matched <- FALSE
        title_items <- list()
        
        if (col$has_name) {
          title_items[[length(title_items) + 1]] <- list(value = col$name, lang = "und")
        }
        
        if (col$has_titles) {
          if (is.character(col$titles)) {
            for (t in col$titles) {
              title_items[[length(title_items) + 1]] <- list(value = t, lang = "und")
            }
          } else if (is.list(col$titles)) {
            if (is.null(names(col$titles))) {
              for (item in col$titles) {
                if (is.character(item)) {
                  title_items[[length(title_items) + 1]] <- list(value = item, lang = "und")
                } else if (is.list(item)) {
                  val <- item[["@value"]]
                  lang <- if (!is.null(item[["@language"]])) item[["@language"]] else "und"
                  title_items[[length(title_items) + 1]] <- list(value = val, lang = lang)
                }
              }
            } else {
              for (lang in names(col$titles)) {
                val <- col$titles[[lang]]
                if (is.character(val)) {
                  for (v in val) {
                    title_items[[length(title_items) + 1]] <- list(value = v, lang = lang)
                  }
                } else if (is.list(val)) {
                  for (v in val) {
                    title_items[[length(title_items) + 1]] <- list(value = v, lang = lang)
                  }
                }
              }
            }
          }
        }
        
        for (item in title_items) {
          if (item$value == hname && languages_match(item$lang, table_lang)) {
            matched <- TRUE
            break
          }
        }
        if (!matched) {
          if (validate && !lax) {
            msg <- sprintf("Column header '%s' is not compatible with metadata column titles or name", hname)
            stop(msg)
          } else {
            col <- parse_column(list(name = hname), generated = TRUE)
          }
        }
      }
    }
    
    if (is.null(col$name) || col$name == "") {
      col$name <- paste0("_col.", j)
    }
    col$header <- hname
    col$parent <- table
    matched_cols[[j]] <- col
  }
  
  rows <- list()
  if (nrow(raw_df) > 0) {
    nrow_df <- nrow(raw_df)
    ncol_raw <- ncol(raw_df)
    col_vecs <- lapply(raw_df, as.character)
    
    # Precompute column-level properties to avoid resolving them in the loops
    col_trim_opts <- lapply(matched_cols, function(col) {
      trim_opt <- inherit_val(col, "trim")
      if (is.null(trim_opt)) dialect$trim else trim_opt
    })
    
    col_null_vals <- lapply(matched_cols, inherit_null)
    col_has_null_spec <- sapply(col_null_vals, function(null_vals) {
      !is.null(null_vals) && !identical(null_vals, "") && !identical(null_vals, list(""))
    })
    
    col_required <- sapply(matched_cols, function(col) isTRUE(col$required))
    col_defaults <- lapply(matched_cols, function(col) col$default)
    col_seps <- lapply(matched_cols, function(col) col$separator)
    col_datatypes <- lapply(matched_cols, function(col) col$datatype)
    col_names <- sapply(matched_cols, function(col) col$name)
    
    # Vectorized trimming function
    trim_value_vector <- function(val_str, trim_opt, skip_initial_space) {
      if (is.null(val_str) || length(val_str) == 0) return(val_str)
      if (isTRUE(skip_initial_space) || identical(skip_initial_space, "true")) {
        val_str <- sub("^\\s+", "", val_str)
      }
      if (identical(trim_opt, TRUE) || identical(trim_opt, "true") || identical(trim_opt, "start")) {
        val_str <- sub("^\\s+", "", val_str)
      }
      if (identical(trim_opt, TRUE) || identical(trim_opt, "true") || identical(trim_opt, "end")) {
        val_str <- sub("\\s+$", "", val_str)
      }
      return(val_str)
    }
    
    # Vectorized preprocessing per column
    parsed_cols <- vector("list", length(matched_cols))
    for (j in seq_along(matched_cols)) {
      col_vals <- if (j <= ncol_raw) col_vecs[[j]] else rep(NA_character_, nrow_df)
      col_vals <- trim_value_vector(col_vals, col_trim_opts[[j]], dialect$skipInitialSpace)
      
      # Determine which indices are null
      is_null_val <- is.na(col_vals)
      if (col_has_null_spec[[j]]) {
        is_null_val <- is_null_val | (col_vals %in% col_null_vals[[j]])
      } else {
        is_null_val <- is_null_val | (col_vals == "")
      }
      
      # Apply default values
      col_default <- col_defaults[[j]]
      if (!is.null(col_default)) {
        col_vals[is_null_val] <- col_default
        is_null_val[is_null_val] <- FALSE
      }
      
      # Required checks
      if (col_required[j] && any(is_null_val)) {
        first_violation <- which(is_null_val)[1]
        msg <- sprintf("Required column '%s' has null value at row %d", col_names[j], first_violation)
        if (strict) {
          stop(msg)
        } else {
          warning(msg)
        }
      }
      
      # Parse only non-null values
      parsed_col_vals <- vector("list", nrow_df)
      non_null_indices <- which(!is_null_val)
      if (length(non_null_indices) > 0) {
        non_null_vals <- col_vals[non_null_indices]
        col_sep <- col_seps[[j]]
        col_dt <- col_datatypes[[j]]
        if (!is.null(col_sep) && col_sep != "") {
          parsed_non_null <- lapply(non_null_vals, function(val_str) {
            parts <- strsplit(val_str, col_sep, fixed = TRUE)[[1]]
            lapply(parts, parse_cell, dt = col_dt, strict = FALSE, validate = validate)
          })
        } else {
          parsed_non_null <- lapply(non_null_vals, parse_cell, dt = col_dt, strict = FALSE, validate = validate)
        }
        parsed_col_vals[non_null_indices] <- parsed_non_null
      }
      parsed_cols[[j]] <- parsed_col_vals
    }
    
    # Construct rows
    source_rows <- 1:nrow_df + skip + (if (dialect$header) dialect$headerRowCount else 0)
    rows <- vector("list", nrow_df)
    has_virtual <- length(virtual_cols) > 0
    for (i in 1:nrow_df) {
      row_context <- list()
      row_context[["_row"]] <- i
      row_context[["_sourceRow"]] <- source_rows[i]
      
      for (j in seq_along(matched_cols)) {
        val <- parsed_cols[[j]][[i]]
        if (!is.null(val)) {
          row_context[[col_names[j]]] <- val
        }
      }
      
      # Handle virtual columns
      if (has_virtual) {
        for (v_idx in seq_along(virtual_cols)) {
          col <- virtual_cols[[v_idx]]
          valUrl <- col$valueUrl
          if (!is.null(valUrl)) {
            cell_context <- row_context
            cell_context[["_name"]] <- col$name
            v_col_idx <- length(regular_cols) + v_idx
            cell_context[["_column"]] <- v_col_idx
            cell_context[["_sourceColumn"]] <- v_col_idx
            row_context[[col$name]] <- expand_uri_template(valUrl, cell_context)
          } else {
            row_context[[col$name]] <- col$default
          }
        }
      }
      
      rows[[i]] <- row_context
    }
  }
  
  rows
}

#' Check uniqueness of primary key columns in table rows
#' @param table csvw_table object
#' @param rows list of row structures
#' @return logical
check_primary_key <- function(table, rows) {
  schema <- table$tableSchema
  if (is.null(schema) || is.null(schema$primaryKey)) return(TRUE)
  
  pk_cols <- schema$primaryKey
  success <- TRUE
  
  # Vectorized extraction of primary keys
  pk_vals_list <- lapply(pk_cols, function(col_name) {
    sapply(rows, function(row) {
      val <- row[[col_name]]
      if (is.null(val)) "NULL" else as.character(val)
    })
  })
  
  pk_strs <- do.call(paste, c(pk_vals_list, sep = "||"))
  
  # Check for nulls
  for (j in seq_along(pk_cols)) {
    null_idx <- which(pk_vals_list[[j]] == "NULL")
    if (length(null_idx) > 0) {
      success <- FALSE
      if (length(null_idx) > 100) {
        warning(sprintf("Primary key column '%s' has %d null values", pk_cols[j], length(null_idx)))
      } else {
        for (idx in null_idx) {
          warning(sprintf("Primary key column '%s' is null at row %d", pk_cols[j], idx))
        }
      }
    }
  }
  
  # Check for duplicates using R's fast C-level duplicated()
  dup_flags <- duplicated(pk_strs)
  if (any(dup_flags)) {
    success <- FALSE
    dup_strs <- unique(pk_strs[dup_flags])
    if (length(dup_strs) > 100) {
      warning(sprintf("Primary key columns have %d duplicate key values", length(dup_strs)))
    } else {
      for (d_str in dup_strs) {
        warning(paste("duplicate primary key:", d_str))
      }
    }
  }
  
  return(success)
}

#' Validate foreign key constraints in table group
#' @param t_obj csvw_table_group object
#' @param strict logical, if TRUE raises warning/error for nulls in composite foreign keys
#' @return logical
#' @export
check_referential_integrity <- function(t_obj, strict = FALSE) {
  tables <- if (inherits(t_obj, "csvw_table_group")) t_obj$tables else list(t_obj)
  table_dict <- list()
  for (tbl in tables) {
    tbl_name <- basename(tbl$url)
    table_dict[[tbl_name]] <- tbl
  }
  
  success <- TRUE
  
  for (tbl in tables) {
    schema <- tbl$tableSchema
    if (is.null(schema) || is.null(schema$foreignKeys)) next
    
    tbl_rows <- tbl$data
    
    for (fk in schema$foreignKeys) {
      source_cols <- fk$columnReference
      ref <- fk$reference
      ref_resource <- ref$resource
      ref_cols <- ref$columnReference
      
      target_tbl <- NULL
      if (!is.null(ref_resource) && ref_resource != "") {
        target_tbl_name <- basename(ref_resource)
        target_tbl <- table_dict[[target_tbl_name]]
      } else if (!is.null(ref$schemaReference) && ref$schemaReference != "") {
        ref_schema_name <- basename(ref$schemaReference)
        for (cand_tbl in tables) {
          cand_schema <- cand_tbl$tableSchema
          if (!is.null(cand_schema)) {
            cand_id <- if (!is.null(cand_schema$id)) basename(cand_schema$id) else ""
            cand_url <- if (!is.null(cand_schema$schema_url)) basename(cand_schema$schema_url) else ""
            if (cand_id == ref_schema_name || cand_url == ref_schema_name) {
              target_tbl <- cand_tbl
              break
            }
          }
        }
      } else {
        target_tbl <- table_dict[[basename(tbl$url)]]
      }
      if (is.null(target_tbl)) {
        stop(paste("Foreign key error: missing table referenced by resource/schemaReference:", 
                   if (!is.null(ref_resource)) ref_resource else ref$schemaReference))
      }
      
      target_schema <- target_tbl$tableSchema
      if (!is.null(target_schema)) {
        for (rc in ref_cols) {
          col_idx <- which(sapply(target_schema$columns, function(c) c$name) == rc)
          if (length(col_idx) > 0) {
            col <- target_schema$columns[[col_idx[1]]]
            if (!col$has_name) {
              stop(sprintf("Referenced target column '%s' must have an explicit name property", rc))
            }
          }
        }
      }
      
      target_rows <- target_tbl$data
      
      # Collect target unique keys vectorially
      t_vals_list <- lapply(ref_cols, function(cname) {
        sapply(target_rows, function(t_row) {
          val <- t_row[[cname]]
          if (is.null(val)) "NULL" else as.character(val)
        })
      })
      target_keys <- do.call(paste, c(t_vals_list, sep = "||"))
      
      # Check uniqueness of target keys
      if (any(duplicated(target_keys))) {
        warning(paste("Foreign key error: referenced columns are not unique in target table", ref_resource))
        success <- FALSE
      }
      
      # Check if any source column is list-valued
      any_list_valued <- FALSE
      for (cname in source_cols) {
        col_idx <- which(sapply(schema$columns, function(c) c$name) == cname)
        if (length(col_idx) > 0) {
          col <- schema$columns[[col_idx[1]]]
          if (!is.null(col$separator) && col$separator != "") {
            any_list_valued <- TRUE
          }
        }
      }
      
      if (!any_list_valued) {
        # Vectorized path
        s_vals_list <- lapply(source_cols, function(cname) {
          sapply(tbl_rows, function(s_row) {
            val <- s_row[[cname]]
            if (is.null(val)) "NULL" else as.character(val)
          })
        })
        s_keys <- do.call(paste, c(s_vals_list, sep = "||"))
        
        all_null_key <- paste(rep("NULL", length(source_cols)), collapse = "||")
        
        # Check null columns
        if (strict) {
          null_indices <- which(s_keys == all_null_key)
          if (length(null_indices) > 0) {
            success <- FALSE
            if (length(null_indices) > 100) {
              warning(sprintf("Foreign key columns are null for %d rows", length(null_indices)))
            } else {
              for (idx in null_indices) {
                warning(paste("Foreign key column is null at row", idx))
              }
            }
          }
        }
        
        # Find mismatched keys (excluding nulls)
        mismatch_indices <- which(!(s_keys %in% target_keys) & s_keys != all_null_key)
        if (length(mismatch_indices) > 0) {
          success <- FALSE
          mismatch_keys <- unique(s_keys[mismatch_indices])
          if (length(mismatch_keys) > 100) {
            warning(sprintf("%d foreign key values not found in table %s", length(mismatch_keys), ref_resource))
          } else {
            for (m_key in mismatch_keys) {
              warning(paste(m_key, "not found in table", ref_resource))
            }
          }
        }
      } else {
        # Fallback to row-by-row loop for list-valued foreign keys
        for (i in seq_along(tbl_rows)) {
          s_row <- tbl_rows[[i]]
          
          col_values <- list()
          for (cname in source_cols) {
            val <- s_row[[cname]]
            is_list_val <- FALSE
            col_idx <- which(sapply(schema$columns, function(c) c$name) == cname)
            if (length(col_idx) > 0) {
              col <- schema$columns[[col_idx[1]]]
              if (!is.null(col$separator) && col$separator != "") {
                is_list_val <- TRUE
              }
            }
            
            if (is.null(val) || (is.list(val) && length(val) == 0)) {
              col_values[[cname]] <- "NULL"
            } else if (is_list_val && is.list(val)) {
              col_values[[cname]] <- sapply(val, function(x) if (is.null(x)) "NULL" else as.character(x))
            } else if (is_list_val && is.vector(val) && length(val) > 1) {
              col_values[[cname]] <- sapply(val, function(x) if (is.null(x)) "NULL" else as.character(x))
            } else {
              col_values[[cname]] <- if (is.null(val)) "NULL" else as.character(val)
            }
          }
          
          grid <- expand.grid(col_values, stringsAsFactors = FALSE)
          grid_keys <- character()
          for (idx in 1:nrow(grid)) {
            row_vals <- as.character(grid[idx, ])
            grid_keys <- c(grid_keys, paste(row_vals, collapse = "||"))
          }
          
          all_null_key <- paste(rep("NULL", length(source_cols)), collapse = "||")
          if (all(grid_keys == all_null_key)) {
            if (strict) {
              warning(paste("Foreign key column is null at row", i))
              success <- FALSE
            }
            next
          }
          
          for (s_key in grid_keys) {
            if (s_key == all_null_key) {
              next
            }
            if (!(s_key %in% target_keys)) {
              warning(paste(s_key, "not found in table", ref_resource))
              success <- FALSE
            }
          }
        }
      }
    }
  }
  
  success
}

#' CSVW constructor
#' @param url path or URL of CSV file
#' @param md_url optional explicit metadata path or URL
#' @param validate logical, if TRUE validates constraints and referential integrity
#' @param lax logical, if TRUE ignores unknown metadata properties rather than flagging them
#' @return a csvw S3 object
#' @export
csvw <- function(url, md_url = NULL, validate = FALSE, lax = FALSE) {
  warnings_list <- character()
  
  run_with_warnings <- function() {
    locate_res <- locate_metadata(url, md_url)
    md <- locate_res$md
    no_header <- locate_res$no_header
    is_located <- locate_res$is_located
    
    validate_jsonld_value(md, allow_context = TRUE)
    validate_context(md[["@context"]])
    
    ctx_props <- extract_context_properties(md)
    meta_base_url <- if (is_url(url)) url else dirname(normalizePath(url, mustWork = FALSE))
    base_path <- ctx_props$base
    if (!is.null(base_path)) {
      base_path <- resolve_url(meta_base_url, base_path)
    } else {
      base_path <- meta_base_url
    }
    
    if (is.null(md$lang) && !is.null(ctx_props$lang)) {
      md$lang <- ctx_props$lang
    }
    
    t_obj <- NULL
    if (!is.null(md[["tables"]])) {
      t_obj <- parse_table_group(md, base_path)
    } else {
      t_obj <- parse_table(md, parent = NULL, base_path = base_path)
      if (no_header) {
        t_obj$dialect$header <- FALSE
      }
    }
    
    if (is_located && !is.null(t_obj)) {
      abs_csv_url <- normalize_url_local(url)
      describes <- FALSE
      tables_list <- if (inherits(t_obj, "csvw_table_group")) t_obj$tables else list(t_obj)
      for (tbl in tables_list) {
        if (normalize_url_local(tbl$url) == abs_csv_url) {
          describes <- TRUE
          break
        }
      }
      if (!describes) {
        warning(paste("Located metadata does not describe the CSV file:", url))
        t_obj <- parse_table(list(url = url), parent = NULL, base_path = base_path)
        if (no_header) {
          t_obj$dialect$header <- FALSE
        }
      }
    }
    
    tables_list <- if (inherits(t_obj, "csvw_table_group")) t_obj$tables else list(t_obj)
    
    for (i in seq_along(tables_list)) {
      tbl <- tables_list[[i]]
      tbl_data <- read_table_csv(tbl, strict = !validate, validate = validate, lax = lax)
      
      if (is.null(tbl$tableSchema)) {
        col_names <- NULL
        if (length(tbl_data) > 0) {
          col_names <- setdiff(names(tbl_data[[1]]), c("_row", "_sourceRow"))
        }
        matched_cols <- list()
        if (!is.null(col_names)) {
          for (j in seq_along(col_names)) {
            col <- parse_column(list(name = col_names[j]), generated = TRUE)
            col$header <- col_names[j]
            col$parent <- tbl
            matched_cols[[j]] <- col
          }
        }
        tbl_schema <- list(
          columns = matched_cols,
          primaryKey = NULL,
          foreignKeys = NULL,
          rowTitles = NULL,
          aboutUrl = NULL
        )
        class(tbl_schema) <- "csvw_table_schema"
        tbl$tableSchema <- tbl_schema
      }
      
      if (inherits(t_obj, "csvw_table_group")) {
        t_obj$tables[[i]]$data <- tbl_data
        t_obj$tables[[i]]$tableSchema <- tbl$tableSchema
      } else {
        t_obj$data <- tbl_data
        t_obj$tableSchema <- tbl$tableSchema
      }
      tables_list[[i]]$data <- tbl_data
      tables_list[[i]]$tableSchema <- tbl$tableSchema
    }
    
    tables_list <- if (inherits(t_obj, "csvw_table_group")) t_obj$tables else list(t_obj)
    
    is_valid_flag <- TRUE
    if (validate) {
      for (tbl in tables_list) {
        pk_success <- check_primary_key(tbl, tbl$data)
        if (!pk_success) {
          is_valid_flag <- FALSE
        }
      }
      
      ri_success <- check_referential_integrity(t_obj, strict = !lax)
      if (!ri_success) {
        is_valid_flag <- FALSE
      }
    }
    
    list(t = t_obj, tables = tables_list, is_valid = is_valid_flag)
  }
  
  res_run <- if (validate) {
    withCallingHandlers(
      run_with_warnings(),
      warning = function(w) {
        warnings_list <<- c(warnings_list, w$message)
        invokeRestart("muffleWarning")
      }
    )
  } else {
    run_with_warnings()
  }
  
  res <- list(
    t = res_run$t,
    tables = res_run$tables,
    is_valid = res_run$is_valid && length(warnings_list) == 0,
    warnings = warnings_list
  )
  class(res) <- "csvw"
  res
}

#' S3 Print method for csvw
#' @param x csvw object
#' @param ... extra print args
#' @exportS3Method print csvw
print.csvw <- function(x, ...) {
  cat("CSVW Object\n")
  cat("Valid:", x$is_valid, "\n")
  cat("Tables count:", length(x$tables), "\n")
  for (tbl in x$tables) {
    cat("  Table URL:", tbl$url, "\n")
    if (!is.null(tbl$data)) {
      cat("    Rows loaded:", length(tbl$data), "\n")
    }
  }
  if (length(x$warnings) > 0) {
    cat("Warnings:\n")
    for (w in x$warnings) {
      cat("  -", w, "\n")
    }
  }
}

#' Convert csvw_table to data.frame S3 method
#' @param x csvw_table object
#' @param ... extra args
#' @return data.frame
#' @exportS3Method as.data.frame csvw_table
as.data.frame.csvw_table <- function(x, ...) {
  if (is.null(x$data) || length(x$data) == 0) {
    return(data.frame())
  }
  
  col_names <- names(x$data[[1]])
  col_names <- setdiff(col_names, "_row")
  
  df_cols <- list()
  for (cname in col_names) {
    col_vals <- lapply(x$data, function(row) {
      val <- row[[cname]]
      if (is.null(val)) NA else val
    })
    
    if (any(sapply(col_vals, is.list))) {
      sanitized <- lapply(col_vals, function(x) {
        if (is.list(x)) {
          lapply(x, function(el) if (is.null(el)) NA else el)
        } else {
          if (is.null(x) || (length(x) == 1 && is.na(x))) NA else x
        }
      })
      df_cols[[cname]] <- I(sanitized)
    } else {
      # Preserve class/attributes for Date and POSIXt
      first_val <- col_vals[[1]]
      if (inherits(first_val, "Date")) {
        mapped_dates <- do.call(c, lapply(col_vals, function(v) if (is.null(v) || is.na(v)) as.Date(NA) else v))
        df_cols[[cname]] <- mapped_dates
      } else if (inherits(first_val, "POSIXt")) {
        mapped_posix <- do.call(c, lapply(col_vals, function(v) if (is.null(v) || is.na(v)) as.POSIXct(NA) else v))
        df_cols[[cname]] <- mapped_posix
      } else {
        null_mapped <- sapply(col_vals, function(v) if (length(v) == 0) NA else v)
        df_cols[[cname]] <- null_mapped
      }
    }
  }
  
  as.data.frame(df_cols, stringsAsFactors = FALSE)
}

#' Convert csvw_table_group to a list of data.frames
#' @param x csvw_table_group object
#' @param ... extra args
#' @return list of data.frames
#' @exportS3Method as.data.frame csvw_table_group
as.data.frame.csvw_table_group <- function(x, ...) {
  res <- list()
  for (tbl in x$tables) {
    tbl_name <- basename(tbl$url)
    res[[tbl_name]] <- as.data.frame(tbl)
  }
  res
}

#' Convenient helper to read CSVW tables
#' @param url path or URL
#' @param md_url optional metadata URL or path
#' @return list of data.frames (or single data.frame if only one table exists)
#' @export
read_csvw <- function(url, md_url = NULL) {
  obj <- csvw(url, md_url, validate = FALSE)
  if (inherits(obj$t, "csvw_table_group")) {
    return(as.data.frame(obj$t))
  } else {
    return(as.data.frame(obj$t))
  }
}

#' Write data for all tables in a TableGroup
#' @param tg csvw_table_group or csvw_table
#' @param fname filename for the metadata file
#' @param strict logical, if TRUE raises error if data contains fields not in schema
#' @param ... other args
#' @return number of rows written or NULL
#' @export
write_csvw <- function(tg, fname, strict = FALSE, ...) {
  # Write functionality
  # For this R implementation, we write the metadata as JSON to fname
  # and write the referenced CSV data to the URLs described in the metadata.
  # Let's check if it's a csvw object, and extract the underlying table/tablegroup
  if (inherits(tg, "csvw")) tg <- tg$t
  
  fname <- normalizePath(fname, mustWork = FALSE)
  dir_path <- dirname(fname)
  
  write_table <- function(table) {
    csv_path <- resolve_url(dir_path, basename(table$url))
    data <- table$data
    if (is.null(data)) return(0)
    
    # Get columns
    cols <- table$tableSchema$columns
    non_virtual_cols <- list()
    for (col in cols) {
      if (!col$virtual) non_virtual_cols[[length(non_virtual_cols) + 1]] <- col
    }
    
    col_names <- sapply(non_virtual_cols, function(col) if (!is.null(col$name)) col$name else col$header)
    
    # Write csv data
    # Create a list of row values matching columns
    df_rows <- list()
    for (row in data) {
      row_vals <- list()
      for (col in non_virtual_cols) {
        val <- row[[col$name]]
        # Convert to string format suitable for CSV
        if (is.null(val)) {
          row_vals[[col$name]] <- ""
        } else if (is.logical(val)) {
          row_vals[[col$name]] <- if (val) "true" else "false"
        } else if (inherits(val, "POSIXt")) {
          row_vals[[col$name]] <- format(val, "%Y-%m-%dT%H:%M:%SZ")
        } else if (inherits(val, "Date")) {
          row_vals[[col$name]] <- format(val, "%Y-%m-%d")
        } else if (is.list(val)) {
          # List datatype: collapse with separator
          sep <- if (!is.null(col$separator)) col$separator else " "
          row_vals[[col$name]] <- paste(unlist(val), collapse = sep)
        } else {
          row_vals[[col$name]] <- as.character(val)
        }
      }
      df_rows[[length(df_rows) + 1]] <- row_vals
    }
    
    df <- do.call(rbind, lapply(df_rows, as.data.frame, stringsAsFactors = FALSE))
    
    # Write using readr::write_delim
    dialect <- table$dialect
    if (is.null(dialect)) dialect <- csvw_dialect()
    
    delim <- dialect$delimiter
    if (is.null(delim)) delim <- ","
    
    # Header logic
    col_names_output <- if (dialect$header) col_names else FALSE
    
    readr::write_delim(
      x = df,
      file = csv_path,
      delim = delim,
      col_names = dialect$header,
      quote = "needed",
      progress = FALSE
    )
    return(nrow(df))
  }
  
  if (inherits(tg, "csvw_table_group")) {
    for (tbl in tg$tables) {
      write_table(tbl)
    }
  } else if (inherits(tg, "csvw_table")) {
    write_table(tg)
  }
  
  # Serialize metadata to JSON file
  # In R, convert Table/TableGroup structure back to lists and serialize using jsonlite::write_json
  # Simple serialize function
  serialize_obj <- function(obj) {
    if (is.null(obj)) return(NULL)
    if (inherits(obj, "csvw_column")) {
      res <- list(
        name = obj$name,
        titles = obj$titles,
        datatype = obj$datatype$base,
        separator = obj$separator,
        required = if (obj$required) TRUE else NULL,
        virtual = if (obj$virtual) TRUE else NULL,
        propertyUrl = obj$propertyUrl,
        valueUrl = obj$valueUrl,
        default = if (obj$default != "") obj$default else NULL
      )
      # Remove NULLs
      return(res[!sapply(res, is.null)])
    }
    if (inherits(obj, "csvw_table_schema")) {
      res <- list(
        columns = lapply(obj$columns, serialize_obj),
        primaryKey = obj$primaryKey,
        foreignKeys = obj$foreignKeys,
        rowTitles = obj$rowTitles
      )
      return(res[!sapply(res, is.null)])
    }
    if (inherits(obj, "csvw_dialect")) {
      res <- list(
        commentPrefix = obj$commentPrefix,
        delimiter = obj$delimiter,
        doubleQuote = obj$doubleQuote,
        encoding = obj$encoding,
        header = obj$header,
        headerRowCount = obj$headerRowCount,
        quoteChar = obj$quoteChar,
        skipBlankRows = obj$skipBlankRows,
        skipColumns = obj$skipColumns,
        skipInitialSpace = obj$skipInitialSpace,
        skipRows = obj$skipRows,
        trim = obj$trim
      )
      return(res[!sapply(res, is.null)])
    }
    if (inherits(obj, "csvw_table")) {
      res <- list(
        url = basename(obj$url),
        tableSchema = serialize_obj(obj$tableSchema),
        dialect = serialize_obj(obj$dialect),
        suppressOutput = if (obj$suppressOutput) TRUE else NULL
      )
      res <- c(res, obj$common_properties)
      return(res[!sapply(res, is.null)])
    }
    if (inherits(obj, "csvw_table_group")) {
      res <- list(
        tables = lapply(obj$tables, serialize_obj),
        dialect = serialize_obj(obj$dialect)
      )
      res <- c(res, obj$common_properties)
      return(res[!sapply(res, is.null)])
    }
  }
  
  json_list <- serialize_obj(tg)
  jsonlite::write_json(json_list, fname, auto_unbox = TRUE, pretty = TRUE)
  return(NULL)
}

#' Validator function for CSVW
#' @param url path or URL
#' @param md_url optional metadata path or URL
#' @param lax logical, if TRUE ignores metadata properties that do not strictly comply
#' @return logical TRUE if valid
#' @export
validate_csvw <- function(url, md_url = NULL, lax = FALSE) {
  obj <- csvw(url, md_url, validate = TRUE, lax = lax)
  return(obj$is_valid)
}

#' Access a table by the last component of its URL
#' @param x a csvw object
#' @param name character string of the last component of the table's URL (e.g. filename)
#' @param ... extra args
#' @return data.frame containing the table rows, or NULL if not found
#' @export
get_table <- function(x, name, ...) {
  UseMethod("get_table")
}

#' @exportS3Method get_table csvw
get_table.csvw <- function(x, name, ...) {
  for (tbl in x$tables) {
    if (identical(basename(tbl$url), name)) {
      return(as.data.frame(tbl))
    }
  }
  return(NULL)
}

