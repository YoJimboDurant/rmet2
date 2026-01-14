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
  
  rmetObj$state$qa$aerminute <- list(
    executed = TRUE,
    qa_status = c(
      run = TRUE,
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
  reason <- classifyAERMINUTEDiagnostics(qa)
  
  can_proceed <- exec$status == "pass" && struct$status != "fail"
  
  list(
    execution = exec,
    structural = struct,
    reasonableness = reason,  # kept for API stability
    summary = list(
      can_proceed = can_proceed,
      overall_status = if (!can_proceed) {
        "fail"
      } else if (struct$status == "warn") {
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
    unlist(yrsum[which(yrsum$`Total Hours`==" Number of processed hours"),])
  )
  total_chr <- as.character(
    unlist(yrsum[which(yrsum$`Total Hours`==" Number of total hours in data period"),])
  )
  
  processed <- as.numeric(stringr::str_extract(processed_chr, "[0-9]+"))
  total <- as.numeric(stringr::str_extract(total_chr, "[0-9]+"))
  
  coverage <- processed / total
  
  coverage <- coverage[2:length(coverage)]
  
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

#' Summarize AERMINUTE diagnostic indicators
#'
#' @param qa Output from qaAerminute().
#'
#' @keywords internal
classifyAERMINUTEDiagnostics <- function(qa) {
  
  messages <- character()
  
  ## ---- Calms ---------------------------------------------------------------
  calms <- qa$qaCalms
  
  calm_lt3 <- .getQaRow(calms, "Number of 1-minute winds < 3 knots")
  calm_ge3 <- .getQaRow(calms, "Number of 1-minute winds >= 3 knots")
  calm_eval <- calm_lt3 + calm_ge3
  
  calm_contradiction <- ifelse(calm_eval > 0, calm_ge3 / calm_eval, NA_real_)
  
  
  if (any(calm_contradiction > 0.10, na.rm = TRUE)) {
    messages <- c(
      messages,
      "Elevated calm-hour contradictions between standard observations and 1-minute winds"
    )
  }
  
  ## ---- Missing winds -------------------------------------------------------
  missing <- qa$qaMissing
  
  miss_total <- .getQaRow(missing, "Number of standard observation missing winds")
  miss_resolved <- .getQaRow(missing, "Number of non-missing 1-minute winds")
  
  miss_resolvable <- ifelse(miss_total > 0, miss_resolved / miss_total, NA_real_)
  
  
  ## ---- Valid wind agreement ------------------------------------------------
  ws <- qa$qaValWS
  
  ws0   <- .getQaRow(ws, "0")
  ws02  <- .getQaRow(ws, "<= 0.2")
  ws05  <- .getQaRow(ws, "0.3 - 0.5")
  ws10  <- .getQaRow(ws, "0.5 - 1")  # include or not based on your cutoff
  
  ws_total <- .getQaColSums(ws)
  ws_within_05 <- (ws0 + ws02 + ws05) / ws_total
  
  wd <- qa$qaValWD
  
  wd0  <- .getQaRow(wd, "0")
  wd10 <- .getQaRow(wd, "<= 10")
  
  wd_total <- .getQaColSums(wd)
  wd_within_10 <- (wd0 + wd10) / wd_total

  if (any(ws_within_05 < 0.90, na.rm = TRUE)) {
    messages <- c(
      messages,
      "Lower-than-typical agreement between standard and 1-minute wind speeds"
    )
  }
  
  if (any(wd_within_10 < 0.90, na.rm = TRUE)) {
    messages <- c(
      messages,
      "Lower-than-typical agreement between standard and 1-minute wind directions"
    )
  }
  
  ## ---- Monthly availability ------------------------------------------------
  monthly <- qa$monthlyData
  monthly_diag <- .analyzeAERMINUTEMonthlyAvailability(monthly)
  if (length(monthly_diag$messages) > 0) {
    messages <- c(messages, monthly_diag$messages)
  }
  
  
  list(
    status = if (length(messages) > 0) "warn" else "pass",
    calm_contradiction = calm_contradiction,
    missing_resolvable = miss_resolvable,
    ws_within_0_5kt = ws_within_05,
    wd_within_10deg = wd_within_10,
    monthly = monthly_diag,
    messages = messages
  )
}

#' Analyze monthly availability for AERMINUTE outputs
#'
#' @param monthlyData Data frame from qaAerminute() $monthlyData.
#' @param warn Monthly coverage threshold to flag for review.
#' @param severe Monthly coverage threshold for major gaps.
#' @param consecutive_n Number of consecutive months below warn to flag.
#'
#' @return List with monthly coverage, flagged months, and review messages.
#' @keywords internal
.analyzeAERMINUTEMonthlyAvailability <- function(
    monthlyData,
    warn = 0.75,
    severe = 0.50,
    consecutive_n = 2
) {
  stopifnot(is.data.frame(monthlyData))
  
  # normalize names defensively (qaAerminute uses spaces in headers sometimes)
  nms <- names(monthlyData)
  # expected: YEAR, MONTH, TOTAL HOURS, VALID HOURS, MISSING HOURS, etc.
  total_col <- nms[grepl("^TOTAL\\s*HOURS$", nms, ignore.case = TRUE)][1]
  valid_col <- nms[grepl("^VALID\\s*HOURS$", nms, ignore.case = TRUE)][1]
  year_col  <- nms[grepl("^YEAR$", nms, ignore.case = TRUE)][1]
  month_col <- nms[grepl("^MONTH$", nms, ignore.case = TRUE)][1]
  
  if (any(is.na(c(total_col, valid_col, year_col, month_col)))) {
    return(list(
      monthly_coverage = NA_real_,
      flagged_months = monthlyData[0, , drop = FALSE],
      messages = "Monthly availability could not be evaluated (unexpected column names)."
    ))
  }
  
  total <- suppressWarnings(as.numeric(monthlyData[[total_col]]))
  valid <- suppressWarnings(as.numeric(monthlyData[[valid_col]]))
  cov <- ifelse(total > 0, valid / total, NA_real_)
  
  # Identify flagged months
  is_severe <- !is.na(cov) & cov < severe
  is_warn   <- !is.na(cov) & cov < warn
  
  flagged <- monthlyData[is_warn, c(year_col, month_col, total_col, valid_col), drop = FALSE]
  if (nrow(flagged) > 0) {
    flagged$coverage <- cov[is_warn]
  }
  
  # Consecutive-month detection (within the table order)
  run_lengths <- rle(is_warn)
  run_ends <- cumsum(run_lengths$lengths)
  run_starts <- run_ends - run_lengths$lengths + 1
  consec_runs <- which(run_lengths$values & run_lengths$lengths >= consecutive_n)
  
  messages <- character()
  
  # Individual month messages
  if (any(is_severe)) {
    idx <- which(is_severe)
    for (i in idx) {
      messages <- c(
        messages,
        sprintf("Major monthly data gap: %s %s (%.1f%% coverage)",
                monthlyData[[month_col]][i], monthlyData[[year_col]][i], cov[i] * 100)
      )
    }
  } else if (any(is_warn)) {
    idx <- which(is_warn)
    for (i in idx) {
      messages <- c(
        messages,
        sprintf("Reduced monthly data availability: %s %s (%.1f%% coverage)",
                monthlyData[[month_col]][i], monthlyData[[year_col]][i], cov[i] * 100)
      )
    }
  }
  
  # Consecutive gap message(s)
  if (length(consec_runs) > 0) {
    for (k in consec_runs) {
      s <- run_starts[k]
      e <- run_ends[k]
      messages <- c(
        messages,
        sprintf("Sustained monthly gap: %s %s through %s %s (%d consecutive months < %.0f%%)",
                monthlyData[[month_col]][s], monthlyData[[year_col]][s],
                monthlyData[[month_col]][e], monthlyData[[year_col]][e],
                e - s + 1, warn * 100)
      )
    }
  }
  
  list(
    monthly_coverage = cov,
    flagged_months = flagged,
    messages = messages
  )
}



writeAERMINUTEQAReport <- function(
    rmetObj,
    file = file.path(rmetObj$project_Dir, "output", "QA_AERMINUTE.txt")
) {
  
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  
  qa <- rmetObj$outputs$aerminute$qa
  qa_class <- rmetObj$outputs$aerminute$qa_class
  diag <- qa_class$reasonableness
  
  con <- file(file, "wt")
  on.exit(close(con))
  
  wl <- function(...) writeLines(paste0(...), con)
  
  wl("AERMINUTE QUALITY ASSURANCE AND DIAGNOSTIC REPORT")
  wl("================================================")
  wl("Generated: ", Sys.Date())
  wl("")
  
  ## ---- Summary -------------------------------------------------------------
  wl("SUMMARY")
  wl("-------")
  wl("Execution status:   ", toupper(qa_class$execution$status))
  wl("Availability status:", toupper(qa_class$structural$status))
  wl("Overall assessment:", toupper(qa_class$summary$overall_status))
  wl("")
  
  ## ---- Availability --------------------------------------------------------
  wl("DATA AVAILABILITY")
  wl("-----------------")
  wl(qa_class$structural$messages)
  wl("")
  
  ## ---- Calms ---------------------------------------------------------------
  wl("CALM-HOUR CONSISTENCY")
  wl("---------------------")
  capture.output(qa$qaCalms, file = con)
  wl("")
  wl("Calm-hour contradiction fractions:")
  wl(paste(sprintf("  %.1f%%", diag$calm_contradiction * 100), collapse = ", "))
  wl("")
  
  ## ---- Missing winds -------------------------------------------------------
  wl("MISSING WIND DIAGNOSTICS")
  wl("------------------------")
  capture.output(qa$qaMissing, file = con)
  wl("")
  wl("Fraction of missing standard winds resolvable by 1-minute data:")
  wl(paste(sprintf("  %.1f%%", diag$missing_resolvable * 100), collapse = ", "))
  wl("")
  
  ## ---- Valid winds ---------------------------------------------------------
  wl("VALID WIND AGREEMENT")
  wl("--------------------")
  wl("Wind speed agreement (≤ 0.5 kt):")
  wl(paste(sprintf("  %.1f%%", diag$ws_within_0_5kt * 100), collapse = ", "))
  wl("")
  wl("Wind direction agreement (≤ 10 degrees):")
  wl(paste(sprintf("  %.1f%%", diag$wd_within_10deg * 100), collapse = ", "))
  wl("")
  
  ## ---- Monthly availability ------------------------------------------------
  wl("MONTHLY DATA AVAILABILITY")
  wl("--------------------------")
  
  flagged <- diag$monthly$flagged_months
  if (is.data.frame(flagged) && nrow(flagged) > 0) {
    
    flagged_fmt <- flagged
    flagged_fmt[["Coverage (%)"]] <- sprintf("%.1f%%", flagged_fmt$coverage * 100)
    flagged_fmt$coverage <- NULL
    
    wl("Flagged months (coverage below threshold):")
    capture.output(flagged_fmt, file = con)
    
  } else {
    wl("No individual months fell below the review threshold.")
  }
  wl("")

  wl("Full monthly availability table:")
  capture.output(qa$monthlyData, file = con)
  wl("")
  
  
  ## ---- Reviewer flags ------------------------------------------------------
  wl("ITEMS FOR REVIEW")
  wl("----------------")
  if (length(diag$messages) == 0) {
    wl("No diagnostic indicators requiring additional review were identified.")
  } else {
    for (m in diag$messages) {
      wl("- ", m)
    }
  }
  
  invisible(file)
}


#' Extract numeric vector from an AERMINUTE QA table by label in first column
#'
#' @param tbl A data.frame returned by qaAerminute(), where the first column
#'   contains row labels and remaining columns are year counts.
#' @param label Character; label to match (exact match by default).
#' @param ignore_case Logical; if TRUE, match case-insensitively.
#'
#' @return Named numeric vector (names are the year columns).
#' @keywords internal
.getQaRow <- function(tbl, label, ignore_case = FALSE) {
  stopifnot(is.data.frame(tbl))
  key_col <- tbl[[1]]
  
  if (ignore_case) {
    idx <- which(tolower(trimws(key_col)) == tolower(trimws(label)))
  } else {
    idx <- which(trimws(key_col) == trimws(label))
  }
  
  if (length(idx) != 1) {
    # Return NA vector with correct length/names to keep downstream code stable
    out <- rep(NA_real_, ncol(tbl) - 1)
    names(out) <- names(tbl)[-1]
    return(out)
  }
  
  out_chr <- as.character(unlist(tbl[idx, -1, drop = TRUE]))
  out_num <- suppressWarnings(as.numeric(out_chr))
  names(out_num) <- names(tbl)[-1]
  out_num
}

#' Sum numeric columns (excluding label column) of an AERMINUTE QA table
#'
#' @param tbl A data.frame returned by qaAerminute().
#' @return Named numeric vector (names are year columns).
#' @keywords internal
.getQaColSums <- function(tbl) {
  vals <- lapply(tbl[-1], function(x) suppressWarnings(as.numeric(as.character(x))))
  out <- colSums(as.data.frame(vals), na.rm = TRUE)
  out
}

