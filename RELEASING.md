# Release Instructions for `rcsvw`

This document provides a checklist and step-by-step guide for releasing the `rcsvw` package on CRAN.

---

## Pre-Release Checklist

Before submitting to CRAN, ensure the following steps are performed locally:

### 1. Document the Package

Regenerate documentation files (`.Rd`) and the `NAMESPACE` if modifications have been made:
```R
devtools::document()
```

### 2. Run Local Unit Tests

Verify that all unit tests pass with zero failures:
```R
devtools::test()
```

### 3. Run CRAN Checks Locally

Ensure that R CMD check passes cleanly on the local environment. It must result in `0 errors` and `0 warnings`.
```R
devtools::check(args = "--as-cran")
```


### 4. Check on Windows Devel/Release

Submit the package to the CRAN Win-Builder service to ensure compatibility on Windows platforms (mandatory for CRAN submission):
```R
devtools::check_win_devel()
devtools::check_win_release()
```
*(Wait for the emails from Win-Builder and confirm there are no errors or warnings.)*


### 5. Check Spelling

Perform a spell check on documentation and description fields:
```R
devtools::spell_check()
```

### 6. Update `NEWS.md`
Ensure `NEWS.md` has a record of changes for the release version (e.g. `0.1.0`).

---

## Build and Submission

Once the pre-release checklist is clean, submit the package to CRAN:

### 1. Build the Source Tarball
Build the package source file (`.tar.gz`):
```R
devtools::build()
```

### 2. Submit to CRAN
You can submit directly from R:
```R
devtools::release()
```
Alternatively, upload the built `.tar.gz` file manually using the [CRAN Web Submission Form](https://cran.r-project.org/submit.html).

### 3. Confirm Submission
- Check your email inbox for a validation link from CRAN.
- Click the link to confirm the submission.
- Monitor the package's CRAN submission queue status.
