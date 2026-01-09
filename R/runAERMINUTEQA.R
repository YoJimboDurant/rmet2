#' Run QA and classification for AERMINUTE outputs
#'
#' Runs raw AERMINUTE QA, classifies results into execution, structural,
#' and reasonableness categories, attaches QA results to the rmet object,
#' updates processing state, prints a concise summary, and writes a
#' human-readable QA report to the output directory.
#'
#' @param rmetObj An rmet object with completed AERMINUTE processing.
#' @param min_coverage_warn Minimum processed-hour coverage to trigger a QA warning.
#' @param min_coverage_fail Minimum processed-hour coverage to trigger a QA failure.
#' @param quiet Logical; if TRUE, suppress console messages.
#' @param write_report Logical; if TRUE, write QA_AERMINUTE.txt to output directory.
#'
#' @return The updated rmet object with QA results attached.
#'
#' @export
runAERMINUTEQA <- function(
    rmetObj,
    min_coverage_warn = 0.90,
    min_coverage_fail = 0.80,
    quiet = FALSE,
    write_report = TRUE
) {
  
  if (min_coverage_fail >= min_coverage_warn) {
    stop("min_coverage_fail must be less than min_coverage_warn.")
  }
  
  if (is.null(rmetObj$project_Dir)) {
    stop("rmetObj$project_Dir is not set.")
  }
  
  years <- locYears(rmetObj)
  if (length(years) == 0) {
    stop("No years found for AERMINUTE QA.")
  }
  
  .assertAERMINUTEArtifacts(rmetObj$project_Dir, years)
  
  qa_raw <- qaAerminute(rmetObj)
  
  qa_class <- classifyAERMINUTEQA(
    qa_raw,
    min_coverage_warn = min_coverage_warn,
    min_coverage_fail = min_coverage_fail
  )
  
  if (is.null(rmetObj$outputs)) rmetObj$outputs <- list()
  if (is.null(rmetObj$outputs$aerminute)) rmetObj$outputs$aerminute <- list()
  
  rmetObj$outputs$aerminute$years <- years
  rmetObj$outputs$aerminute$qa <- qa_raw
  rmetObj$outputs$aerminute$qa_class <- qa_class
  
  rmetObj$state$aerminute <- list(
    executed = TRUE,
    qa_status = c(
      execution = qa_class$execution$status,
      structural = qa_class$structural$status,
      reasonableness = qa_class$reasonableness$status
    ),
    can_proceed = qa_class$summary$can_proceed
  )
  
  if (!quiet) {
    message("AERMINUTE QA SUMMARY")
    message("  Execution:     ", toupper(qa_class$execution$status))
    message("  Structural:    ", toupper(qa_class$structural$status))
    message("  Reasonableness:", toupper(qa_class$reasonableness$status))
    if (length(qa_class$reasonableness$messages) > 0) {
      message("  Notes:")
      for (m in qa_class$reasonableness$messages) {
        message("   - ", m)
      }
    }
  }
  
  if (write_report) {
    writeAERMINUTEQAReport(rmetObj)
  }
  
  rmetObj
}

#' Assert required AERMINUTE artifacts exist
#'
#' @param project_Dir Base project directory.
#' @param years Integer vector of expected years.
#'
#' @keywords internal
.assertAERMINUTEArtifacts <- function(project_Dir, years) {
  
  log_paths <- file.path(project_Dir, years, "aerminute.log")
  missing <- log_paths[!file.exists(log_paths)]
  
  if (length(missing) > 0) {
    stop(
      "Missing AERMINUTE artifacts:\n",
      paste(missing, collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

#' Classify AERMINUTE QA results
#'
#' @param qa Output from qaAerminute().
#' @param min_coverage_warn Coverage threshold for warning.
#' @param min_coverage_fail Coverage threshold for failure.
#'
#' @keywords internal
classifyAERMINUTEQA <- function(
    qa,
    min_coverage_warn,
    min_coverage_fail
) {
  
  exec <- classifyAERMINUTEExecution(qa)
  struct <- classifyAERMINUTEStructural(
    qa,
    min_warn = min_coverage_warn,
    min_fail = min_coverage_fail
  )
  reason <- classifyAERMINUTEReasonableness(qa)
  
  can_proceed <- exec$status == "pass" && struct$status != "fail"
  
  list(
    execution = exec,
    structural = struct,
    reasonableness = reason,
    summary = list(
      can_proceed = can_proceed,
      overall_status = if (!can_proceed) {
        "fail"
      } else if (any(c(exec$status, struct$status, reason$status) == "warn")) {
        "warn"
      } else {
        "pass"
      }
    )
  )
}

#' Classify AERMINUTE execution QA
#'
#' @param qa Output from qaAerminute().
#'
#' @keywords internal
classifyAERMINUTEExecution <- function(qa) {
  
  rec <- qa$RecCheck
  idx <- grep("bad minute", rownames(rec), ignore.case = TRUE)
  
  bad_minute <- if (length(idx) > 0) {
    sum(as.numeric(unlist(rec[idx, ])), na.rm = TRUE)
  } else {
    0
  }
  
  list(
    status = ifelse(bad_minute == 0, "pass", "fail"),
    messages = if (bad_minute == 0) {
      character()
    } else {
      "Bad minute records detected"
    }
  )
}

#' Classify AERMINUTE structural QA
#'
#' @param qa Output from qaAerminute().
#' @param min_warn Coverage threshold for warning.
#' @param min_fail Coverage threshold for failure.
#'
#' @keywords internal
classifyAERMINUTEStructural <- function(
    qa,
    min_warn,
    min_fail
) {
  
  yrsum <- qa$YearNumSum
  
  processed_chr <- as.character(
    unlist(yrsum["Number of processed hours", ])
  )
  total_chr <- as.character(
    unlist(yrsum["Number of total hours in data period", ])
  )
  
  processed <- as.numeric(gsub("[^0-9]", "", processed_chr))
  total <- as.numeric(gsub("[^0-9]", "", total_chr))
  
  coverage <- processed / total
  
  status <- if (any(coverage < min_fail, na.rm = TRUE)) {
    "fail"
  } else if (any(coverage < min_warn, na.rm = TRUE)) {
    "warn"
  } else {
    "pass"
  }
  
  list(
    status = status,
    coverage = coverage,
    coverage_warn = min_warn,
    coverage_fail = min_fail,
    messages = sprintf(
      "Processed-hour coverage: %s",
      paste(sprintf("%.1f%%", coverage * 100), collapse = ", ")
    )
  )
}

#' Classify AERMINUTE reasonableness QA
#'
#' @param qa Output from qaAerminute().
#'
#' @keywords internal
classifyAERMINUTEReasonableness <- function(qa) {
  
  calms <- qa$qaCalms
  missing <- qa$qaMissing
  
  calm_std <- as.numeric(
    unlist(calms["Number of standard observation calms", ])
  )
  calm_total <- colSums(
    sapply(calms, function(x) as.numeric(x)),
    na.rm = TRUE
  )
  calm_rate <- calm_std / calm_total
  
  miss_std <- as.numeric(
    unlist(missing["Number of standard observation missing winds", ])
  )
  miss_total <- colSums(
    sapply(missing, function(x) as.numeric(x)),
    na.rm = TRUE
  )
  miss_rate <- miss_std / miss_total
  
  messages <- character()
  status <- "pass"
  
  if (any(calm_rate > 0.10, na.rm = TRUE)) {
    status <- "warn"
    messages <- c(messages, "Elevated calm frequency in standard observations")
  }
  
  if (any(miss_rate > 0.05, na.rm = TRUE)) {
    status <- "warn"
    messages <- c(messages, "Elevated missing wind frequency")
  }
  
  list(
    status = status,
    calm_rate = calm_rate,
    missing_rate = miss_rate,
    messages = messages
  )
}

#' Write AERMINUTE QA report
#'
#' @param rmetObj An rmet object with classified AERMINUTE QA results.
#' @param file Output file path.
#'
#' @keywords internal
writeAERMINUTEQAReport <- function(
    rmetObj,
    file = file.path(paste(rmetObj$project_Dir, "output", "QA_AERMINUTE.txt", sep = "/"))
) {
  
  dir.create(paste(rmetObj$project_Dir, "output", sep = "/"), showWarnings = FALSE)
  
  qa_class <- rmetObj$outputs$aerminute$qa_class
  
  con <- file(file, "wt")
  on.exit(close(con))
  
  wl <- function(...) writeLines(paste0(...), con)
  
  wl("AERMINUTE QUALITY ASSURANCE REPORT")
  wl(strrep("=", 34))
  wl("")
  wl("Date generated: ", Sys.Date())
  wl("")
  
  wl("SUMMARY")
  wl("-------")
  wl("Execution status:     ", toupper(qa_class$execution$status))
  wl("Structural status:    ", toupper(qa_class$structural$status))
  wl("Reasonableness status:", toupper(qa_class$reasonableness$status))
  wl("Overall assessment:   ", toupper(qa_class$summary$overall_status))
  wl("")
  
  wl("QA POLICY")
  wl("---------")
  wl(sprintf("- Coverage warning threshold: %.0f%%",
             qa_class$structural$coverage_warn * 100))
  wl(sprintf("- Coverage failure threshold: %.0f%%",
             qa_class$structural$coverage_fail * 100))
  wl("")
  
  invisible(file)
}


