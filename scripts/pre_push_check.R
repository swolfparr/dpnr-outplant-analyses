#!/usr/bin/env Rscript

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)

  repeat {
    if (dir.exists(file.path(current, ".git")) &&
        file.exists(file.path(current, "_quarto.yml"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find the Coral_survivorship_project root.", call. = FALSE)
    }
    current <- parent
  }
}

run_git <- function(args) {
  system2("git", args, stdout = TRUE, stderr = TRUE, env = "GIT_OPTIONAL_LOCKS=0")
}

check_file_exists <- function(paths, root) {
  missing <- paths[!file.exists(file.path(root, paths))]
  if (length(missing) == 0) {
    return(TRUE)
  }

  message("Missing required files:")
  for (path in missing) {
    message("  - ", path)
  }
  FALSE
}

check_audit_file <- function(path, root, label) {
  full_path <- file.path(root, path)
  if (!file.exists(full_path)) {
    message(label, " audit table is missing: ", path)
    return(FALSE)
  }

  audit <- utils::read.csv(full_path, stringsAsFactors = FALSE)
  if (!"file_exists" %in% names(audit)) {
    message(label, " audit table does not contain a file_exists column: ", path)
    return(FALSE)
  }

  missing <- audit[audit$file_exists %in% c(FALSE, "FALSE", "false", 0), , drop = FALSE]
  if (nrow(missing) == 0) {
    return(TRUE)
  }

  file_col <- if ("file" %in% names(missing)) "file" else names(missing)[[1]]
  message(label, " audit has configured files that do not exist:")
  for (file in missing[[file_col]]) {
    message("  - ", file)
  }
  FALSE
}

root <- find_project_root()
setwd(root)

message("Project: ", root)

required_files <- c(
  "_quarto.yml",
  "index.qmd",
  "coral_survivorship_report.qmd",
  "all_coral_cover.qmd",
  "docs/index.html",
  "docs/coral_survivorship_report.html",
  "docs/all_coral_cover.html",
  "outputs/Tables/outplant_master_summary_dataset.csv",
  "outputs/Tables/outplant_cumulative_survival_summary.csv",
  "outputs/Tables/outplant_file_audit.csv",
  "outputs/Figures/outplant_cumulative_survival_plot.png"
)

checks <- c(
  check_file_exists(required_files, root),
  check_audit_file("outputs/Tables/outplant_file_audit.csv", root, "Outplant")
)

all_coral_audit <- "outputs/Tables/all_corals/all_coral_file_audit.csv"
if (file.exists(file.path(root, all_coral_audit))) {
  checks <- c(checks, check_audit_file(all_coral_audit, root, "All-coral"))
}

remote <- tryCatch(run_git(c("remote", "get-url", "origin")), error = function(e) "")
message("Git remote origin: ", remote[[1]])
expected_remote <- grepl("CWORI/DPNR_Survivorship\\.git$", remote[[1]])
if (!expected_remote) {
  message("Remote does not look like CWORI/DPNR_Survivorship.git.")
}
checks <- c(checks, expected_remote)

message("")
message("Git status:")
status <- tryCatch(run_git(c("status", "--short", "--branch")), error = function(e) conditionMessage(e))
message(paste(status, collapse = "\n"))

if (all(checks)) {
  message("")
  message("Pre-push check passed. Review, stage, commit, and push when ready.")
  quit(status = 0)
}

message("")
message("Pre-push check failed. Fix the items above before pushing.")
quit(status = 1)
