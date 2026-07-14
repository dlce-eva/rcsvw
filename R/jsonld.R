# Generic to_json function and standard/minimal modes implementation

#' Shorten a URI using standard context prefixes
#' @param uri character string URI
#' @return shortened character string
#' @noRd
shorten_uri <- function(uri) {
  if (is.null(uri) || !is.character(uri)) return(uri)
  
  prefixes <- list(
    "schema" = "http://schema.org/",
    "xsd" = "http://www.w3.org/2001/XMLSchema#",
    "dc" = "http://purl.org/dc/terms/",
    "dcat" = "http://www.w3.org/ns/dcat#",
    "prov" = "http://www.w3.org/ns/prov#",
    "void" = "http://rdfs.org/ns/void#",
    "csvw" = "http://www.w3.org/ns/csvw#",
    "rdf" = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  )
  
  for (name in names(prefixes)) {
    prefix_val <- prefixes[[name]]
    if (startsWith(uri, prefix_val)) {
      suffix <- substring(uri, nchar(prefix_val) + 1)
      return(paste0(name, ":", suffix))
    }
  }
  return(uri)
}

expand_prefix_uri <- function(uri) {
  if (is.null(uri) || !is.character(uri)) return(uri)
  
  prefixes <- list(
    "schema" = "http://schema.org/",
    "xsd" = "http://www.w3.org/2001/XMLSchema#",
    "dc" = "http://purl.org/dc/terms/",
    "dcat" = "http://www.w3.org/ns/dcat#",
    "prov" = "http://www.w3.org/ns/prov#",
    "void" = "http://rdfs.org/ns/void#",
    "csvw" = "http://www.w3.org/ns/csvw#",
    "rdf" = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  )
  
  for (name in names(prefixes)) {
    prefix_val <- prefixes[[name]]
    if (startsWith(uri, paste0(name, ":"))) {
      suffix <- substring(uri, nchar(name) + 2)
      return(paste0(prefix_val, suffix))
    }
  }
  return(uri)
}

format_json_value <- function(val) {
  if (is.null(val)) return(NULL)
  if (inherits(val, "POSIXt")) {
    return(format(val, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  if (inherits(val, "Date")) {
    return(format(val, "%Y-%m-%d"))
  }
  if (is.list(val)) {
    return(lapply(val, format_json_value))
  }
  return(val)
}

clean_jsonld_value <- function(val, base_url = NULL) {
  if (is.null(val)) return(NULL)
  
  if (is.list(val)) {
    if (!is.null(names(val))) {
      # object/named list
      if ("@value" %in% names(val)) {
        return(val[["@value"]])
      }
      if ("@id" %in% names(val)) {
        uri <- val[["@id"]]
        uri <- expand_prefix_uri(uri)
        if (!is.null(base_url)) {
          return(resolve_url(base_url, uri))
        }
        return(uri)
      }
      cleaned <- lapply(val, clean_jsonld_value, base_url = base_url)
      return(cleaned)
    } else {
      # array/unnamed list
      cleaned <- lapply(val, clean_jsonld_value, base_url = base_url)
      # Simplify atomic vectors if possible, to match jsonlite structure
      if (length(cleaned) > 0 && all(sapply(cleaned, function(x) is.character(x) && length(x) == 1))) {
        return(unlist(cleaned))
      }
      return(cleaned)
    }
  }
  return(val)
}

#' Convert CSVW object to JSON representation
#' @param x csvw object
#' @param minimal logical, if TRUE returns flat list of descriptors, else standard group/table structure
#' @param ... extra args
#' @return list structure ready for JSON serialization
#' @export
to_json <- function(x, minimal = FALSE, ...) {
  UseMethod("to_json")
}

#' Convert csvw S3 object to JSON structure
#' @param x csvw object
#' @param minimal logical
#' @param ... extra args
#' @return list structure
#' @exportS3Method to_json csvw
to_json.csvw <- function(x, minimal = FALSE, ...) {
  t_obj <- x$t
  tables <- x$tables
  
  tables_json <- list()
  for (tbl in tables) {
    if (tbl$suppressOutput) next
    
    tbl_json <- list()
    tbl_json$url <- tbl$url
    
    if (!is.null(tbl$id)) {
      tbl_json[["@id"]] <- clean_jsonld_value(tbl$id, base_url = tbl$url)
    }
    if (!is.null(tbl$notes)) {
      tbl_json[["notes"]] <- clean_jsonld_value(tbl$notes, base_url = tbl$url)
    }
    
    cleaned_common_tbl <- clean_jsonld_value(tbl$common_properties, base_url = tbl$url)
    if (!is.null(cleaned_common_tbl) && length(cleaned_common_tbl) > 0) {
      tbl_json <- c(tbl_json, cleaned_common_tbl)
    }
    
    rows_json <- list()
    if (!is.null(tbl$data)) {
      for (rownum in seq_along(tbl$data)) {
        row <- tbl$data[[rownum]]
        rowsourcenum <- if (!is.null(row[["_sourceRow"]])) row[["_sourceRow"]] else row[["_row"]]
        
        row_json <- list()
        row_json$url <- paste0(tbl$url, "#row=", rowsourcenum)
        row_json$rownum <- rownum
        
        # rowTitles
        rowTitles <- tbl$tableSchema$rowTitles
        if (!is.null(rowTitles)) {
          titles <- list()
          for (rt in rowTitles) {
            if (!is.null(row[[rt]])) titles[[length(titles) + 1]] <- row[[rt]]
          }
          if (length(titles) == 1) {
            row_json$titles <- titles[[1]]
          } else if (length(titles) > 1) {
            row_json$titles <- titles
          }
        }
        
        # Triples extraction
        triples <- list()
        
        # Resolve row aboutUrl
        aboutUrl_tbl <- NULL
        if (!is.null(tbl$tableSchema) && length(tbl$tableSchema$columns) > 0) {
          aboutUrl_tbl <- inherit_val(tbl$tableSchema$columns[[1]], "aboutUrl")
        }
        if (is.null(aboutUrl_tbl) && !is.null(tbl$tableSchema)) {
          aboutUrl_tbl <- tbl$tableSchema$aboutUrl
        }
        
        row_about <- NULL
        if (!is.null(aboutUrl_tbl)) {
          row_about <- expand_uri_template(aboutUrl_tbl, row)
          row_about <- resolve_url(tbl$url, row_about)
        }
        
        # Process cells
        for (col_idx in seq_along(tbl$tableSchema$columns)) {
          col <- tbl$tableSchema$columns[[col_idx]]
          if (col$virtual || col$suppressOutput) next
          
          val <- row[[col$name]]
          if (is.null(val) || identical(val, "") || identical(val, list())) next
          
          cell_context <- row
          cell_context[["_name"]] <- col$name
          cell_context[["_column"]] <- col_idx
          cell_context[["_sourceColumn"]] <- col_idx
          
          # propertyUrl
          propUrl <- inherit_val(col, "propertyUrl")
          prop <- if (!is.null(propUrl)) expand_uri_template(propUrl, cell_context) else col$name
          if (!is.null(propUrl)) {
            prop <- resolve_url(tbl$url, prop)
          }
          prop <- shorten_uri(expand_prefix_uri(prop))
          
          # valueUrl
          valUrl <- inherit_val(col, "valueUrl")
          val_final <- if (!is.null(valUrl)) expand_uri_template(valUrl, cell_context) else val
          if (!is.null(valUrl)) {
            val_final <- resolve_url(tbl$url, val_final)
            val_final <- expand_prefix_uri(val_final)
          }
          if (prop == "rdf:type" || prop == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" || prop == "@type") {
            val_final <- shorten_uri(val_final)
          }
          val_final <- format_json_value(val_final)
          
          # aboutUrl
          col_aboutUrl <- inherit_val(col, "aboutUrl")
          about_final <- if (!is.null(col_aboutUrl)) expand_uri_template(col_aboutUrl, cell_context) else row_about
          if (!is.null(about_final)) {
            about_final <- resolve_url(tbl$url, about_final)
          }
          
          triples[[length(triples) + 1]] <- list(about = about_final, property = prop, value = val_final)
        }
        
        # Process virtual columns
        for (col_idx in seq_along(tbl$tableSchema$columns)) {
          col <- tbl$tableSchema$columns[[col_idx]]
          if (col$virtual) {
            cell_context <- row
            cell_context[["_name"]] <- col$name
            cell_context[["_column"]] <- col_idx
            cell_context[["_sourceColumn"]] <- col_idx
            
            propUrl <- inherit_val(col, "propertyUrl")
            prop <- if (!is.null(propUrl)) expand_uri_template(propUrl, cell_context) else col$name
            if (!is.null(propUrl)) {
              prop <- resolve_url(tbl$url, prop)
            }
            prop <- shorten_uri(expand_prefix_uri(prop))
            
            valUrl <- inherit_val(col, "valueUrl")
            val_final <- if (!is.null(valUrl)) expand_uri_template(valUrl, cell_context) else col$default
            if (!is.null(valUrl)) {
              val_final <- resolve_url(tbl$url, val_final)
              val_final <- expand_prefix_uri(val_final)
            }
            if (prop == "rdf:type" || prop == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" || prop == "@type") {
              val_final <- shorten_uri(val_final)
            }
            val_final <- format_json_value(val_final)
            
            col_aboutUrl <- inherit_val(col, "aboutUrl")
            about_final <- if (!is.null(col_aboutUrl)) expand_uri_template(col_aboutUrl, cell_context) else row_about
            if (!is.null(about_final)) {
              about_final <- resolve_url(tbl$url, about_final)
            }
            
            triples[[length(triples) + 1]] <- list(about = about_final, property = prop, value = val_final)
          }
        }
        
        # Group triples by about subject identifier
        nodes <- list()
        for (tr in triples) {
          about_key <- if (is.null(tr$about)) "anonymous" else tr$about
          if (is.null(nodes[[about_key]])) {
            nodes[[about_key]] <- list()
            if (about_key != "anonymous") {
              nodes[[about_key]][["@id"]] <- about_key
            }
          }
          prop <- tr$property
          val <- tr$value
          
          if (prop == "rdf:type" || prop == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") prop <- "@type"
          
          # Unbox single value or append list
          if (is.null(nodes[[about_key]][[prop]])) {
            nodes[[about_key]][[prop]] <- val
          } else {
            nodes[[about_key]][[prop]] <- c(nodes[[about_key]][[prop]], val)
          }
        }
        
        # Nesting transformation
        node_ids <- setdiff(names(nodes), "anonymous")
        if (!is.null(row_about) && row_about != "") {
          candidate_ids <- setdiff(node_ids, row_about)
        } else {
          candidate_ids <- node_ids
        }
        
        for (child_id in candidate_ids) {
          refs <- list()
          for (parent_key in names(nodes)) {
            p_node <- nodes[[parent_key]]
            for (p_name in setdiff(names(p_node), c("@id", "@type"))) {
              val <- p_node[[p_name]]
              if (is.character(val) && child_id %in% val) {
                refs[[length(refs) + 1]] <- list(parent = parent_key, prop = p_name)
              } else if (is.list(val)) {
                for (val_idx in seq_along(val)) {
                  if (is.character(val[[val_idx]]) && val[[val_idx]] == child_id) {
                    refs[[length(refs) + 1]] <- list(parent = parent_key, prop = p_name, idx = val_idx)
                  }
                }
              }
            }
          }
          
          if (length(refs) == 1) {
            ref_info <- refs[[1]]
            p_key <- ref_info$parent
            p_name <- ref_info$prop
            child_node <- nodes[[child_id]]
            
            if (!is.null(ref_info$idx)) {
              nodes[[p_key]][[p_name]][[ref_info$idx]] <- child_node
            } else {
              val <- nodes[[p_key]][[p_name]]
              if (length(val) == 1) {
                nodes[[p_key]][[p_name]] <- child_node
              } else {
                match_idx <- which(val == child_id)
                if (length(match_idx) > 0) {
                  val_list <- as.list(val)
                  val_list[[match_idx[1]]] <- child_node
                  nodes[[p_key]][[p_name]] <- val_list
                }
              }
            }
            nodes[[child_id]] <- NULL
          }
        }
        
        row_json$describes <- unname(nodes)
        rows_json[[length(rows_json) + 1]] <- row_json
      }
    }
    
    tbl_json$row <- rows_json
    tables_json[[length(tables_json) + 1]] <- tbl_json
  }
  
  if (minimal) {
    flat_describes <- list()
    for (t_j in tables_json) {
      for (r_j in t_j$row) {
        for (d_j in r_j$describes) {
          flat_describes[[length(flat_describes) + 1]] <- d_j
        }
      }
    }
    return(flat_describes)
  }
  
  res <- list(tables = tables_json)
  
  if (inherits(t_obj, "csvw_table_group")) {
    if (!is.null(t_obj$id)) {
      res[["@id"]] <- clean_jsonld_value(t_obj$id, base_url = t_obj$url)
    }
    if (!is.null(t_obj$notes)) {
      res[["notes"]] <- clean_jsonld_value(t_obj$notes, base_url = t_obj$url)
    }
    
    cleaned_common <- clean_jsonld_value(t_obj$common_properties, base_url = t_obj$url)
    if (!is.null(cleaned_common) && length(cleaned_common) > 0) {
      res <- c(cleaned_common, res)
    }
  }
  
  return(res)
}
