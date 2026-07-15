# Utility Functions for rcsvw

#' Check if a string is a URL
#' @param x character string
#' @return logical
is_url <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  grepl("^(http|https|ftp)://", x)
}

#' Resolve path or URL relative to base URL or path
#' @param base character string base URL or directory path
#' @param ref character string relative path or URL
#' @return resolved path or URL
resolve_url <- function(base, ref) {
  if (is.null(ref) || ref == "") return(base)
  if (is_url(ref)) return(ref)
  
  if (startsWith(ref, "#")) {
    if (is_url(base)) {
      base_parsed <- httr::parse_url(base)
      base_parsed$fragment <- substring(ref, 2)
      return(httr::build_url(base_parsed))
    } else {
      return(paste0(base, ref))
    }
  }
  
  if (startsWith(ref, "?")) {
    if (is_url(base)) {
      base_parsed <- httr::parse_url(base)
      ref_parsed <- httr::parse_url(ref)
      base_parsed$query <- ref_parsed$query
      base_parsed$fragment <- ref_parsed$fragment
      return(httr::build_url(base_parsed))
    } else {
      return(paste0(base, ref))
    }
  }
  
  if (grepl("^(/|\\\\|[A-Za-z]:)", ref) && !is_url(base)) {
    return(ref)
  }
  
  if (is_url(base)) {
    # Parse URLs using httr package structures
    base_parsed <- httr::parse_url(base)
    ref_parsed <- httr::parse_url(ref)
    
    if (!is.null(ref_parsed$scheme)) {
      return(ref)
    }

    # A leading slash is relative to the origin, not to the current URL path.
    if (startsWith(ref, "/")) {
      base_parsed$path <- clean_path(ref)
      base_parsed$query <- ref_parsed$query
      base_parsed$fragment <- ref_parsed$fragment
      return(httr::build_url(base_parsed))
    }
    
    base_path <- base_parsed$path
    if (is.null(base_path) || base_path == "") {
      base_path <- "/"
    }
    
    # Remove filename from base_path if it doesn't end in a slash
    if (!grepl("/$", base_path)) {
      parts <- strsplit(base_path, "/")[[1]]
      if (length(parts) > 1) {
        base_path <- paste(parts[-length(parts)], collapse = "/")
      } else {
        base_path <- ""
      }
    }
    
    combined_path <- paste0(base_path, if (base_path == "" || grepl("/$", base_path)) "" else "/", ref)
    combined_path <- clean_path(combined_path)
    
    base_parsed$path <- combined_path
    base_parsed$query <- ref_parsed$query
    base_parsed$fragment <- ref_parsed$fragment
    
    return(httr::build_url(base_parsed))
  } else {
    # Base is local file or dir path
    # If base is a file, get dirname
    if (file.exists(base) && !file.info(base)$isdir) {
      base_dir <- dirname(base)
    } else {
      base_dir <- base
    }
    full_path <- file.path(base_dir, ref)
    return(normalizePath(full_path, mustWork = FALSE))
  }
}

#' Helper to clean path relative movements like /./ and /../
#' @param path path string
#' @return cleaned path
clean_path <- function(path) {
  path <- gsub("/\\./", "/", path)
  while (grepl("/[^/]+/\\.\\./", path)) {
    path <- gsub("/[^/]+/\\.\\./", "/", path)
  }
  path <- gsub("^\\.\\./", "", path)
  path
}

normalize_url_local <- function(url_or_path) {
  if (is.character(url_or_path)) {
    w3c_patterns <- c(
      "http://www.w3.org/2013/csvw/tests/",
      "https://w3c.github.io/csvw/tests/",
      "https://www.w3.org/2013/csvw/tests/"
    )
    for (pat in w3c_patterns) {
      if (startsWith(url_or_path, pat)) {
        local_dir <- NULL
        if (requireNamespace("testthat", quietly = TRUE)) {
          local_dir <- tryCatch({
            testthat::test_path("w3c-tests")
          }, error = function(e) {
            NULL
          })
        }
        if (is.null(local_dir) || !dir.exists(local_dir)) {
          if (dir.exists("tests/testthat/w3c-tests")) {
            local_dir <- "tests/testthat/w3c-tests"
          } else if (dir.exists("w3c-tests")) {
            local_dir <- "w3c-tests"
          } else {
            local_dir <- system.file("tests", "testthat", "w3c-tests", package = "rcsvw")
          }
        }
        if (dir.exists(local_dir)) {
          rel_path <- substring(url_or_path, nchar(pat) + 1)
          rel_path <- sub("#.*$", "", rel_path)
          rel_path <- sub("\\?.*$", "", rel_path)
          local_path <- file.path(local_dir, rel_path)
          if (file.exists(local_path)) {
            return(local_path)
          }
        }
      }
    }
  }

  # Query strings and fragments are URI components, not part of a local
  # filesystem name. Canonicalizing also avoids /tmp versus /private/tmp
  # mismatches on macOS when comparing described resources.
  if (is.character(url_or_path) && length(url_or_path) == 1 && !is_url(url_or_path)) {
    local_path <- sub("[?#].*$", "", url_or_path)
    return(normalizePath(local_path, mustWork = FALSE))
  }

  return(url_or_path)
}

#' Fetch and parse a JSON document from local path or URL
#' @param url_or_path path or URL
#' @return list structure
get_json <- function(url_or_path) {
  target <- normalize_url_local(url_or_path)
  if (is_url(target)) {
    resp <- httr::GET(target)
    httr::stop_for_status(resp)
    content <- httr::content(resp, as = "text", encoding = "UTF-8")
    jsonlite::fromJSON(content, simplifyVector = FALSE)
  } else {
    jsonlite::fromJSON(target, simplifyVector = FALSE)
  }
}

#' Expand RFC 6570 URI template using context variables
#' @param template URI template string
#' @param context list containing variables for expansion
#' @return expanded string
expand_uri_template <- function(template, context) {
  if (is.null(template) || template == "") return(template)
  
  matches <- gregexpr("\\{[^}]+\\}", template)[[1]]
  if (matches[1] == -1) return(template)
  
  match_lengths <- attr(matches, "match.length")
  result <- template
  
  for (i in rev(seq_along(matches))) {
    start <- matches[i]
    len <- match_lengths[i]
    placeholder <- substr(result, start + 1, start + len - 2)
    
    plus <- FALSE
    hash <- FALSE
    varname <- placeholder
    if (startsWith(placeholder, "+")) {
      plus <- TRUE
      varname <- substring(placeholder, 2)
    } else if (startsWith(placeholder, "#")) {
      hash <- TRUE
      varname <- substring(placeholder, 2)
    }
    
    val <- context[[varname]]
    
    # Handle special _row context if available
    if (is.null(val) && varname == "_row" && !is.null(context[["_row"]])) {
      val <- context[["_row"]]
    }
    
    replacement <- ""
    if (!is.null(val) && length(val) > 0) {
      if (is.list(val)) val <- unlist(val)
      val_str <- as.character(val)
      if (plus) {
        # Keep reserved characters unescaped (reserved=FALSE in URLencode)
        replacement <- sapply(val_str, function(v) utils::URLencode(v, reserved = FALSE))
        replacement <- paste(replacement, collapse = ",")
      } else if (hash) {
        # Fragment expansion: prepend '#' and keep reserved characters unescaped
        replacement <- sapply(val_str, function(v) utils::URLencode(v, reserved = FALSE))
        replacement <- paste(replacement, collapse = ",")
        if (replacement != "") {
          replacement <- paste0("#", replacement)
        }
      } else {
        # Escape all reserved characters
        replacement <- sapply(val_str, function(v) utils::URLencode(v, reserved = TRUE))
        replacement <- paste(replacement, collapse = ",")
      }
    }
    
    left_part <- if (start > 1) substr(result, 1, start - 1) else ""
    right_part <- if (start + len <= nchar(result)) substr(result, start + len, nchar(result)) else ""
    result <- paste0(left_part, replacement, right_part)
  }
  
  return(result)
}
