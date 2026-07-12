#!/usr/bin/env Rscript

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "_quarto.yml")) &&
        dir.exists(file.path(current, "config", "outplant_interval_files"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find the Coral_survivorship_project root.", call. = FALSE)
    }
    current <- parent
  }
}

project_root <- find_project_root()
render_script <- file.path(project_root, "scripts", "render_site.sh")

if (!file.exists(render_script)) {
  stop("Could not find scripts/render_site.sh.", call. = FALSE)
}

setwd(project_root)

message("Rendering project through scripts/render_site.sh")
status <- system2("bash", render_script)

if (!identical(status, 0L)) {
  stop("Quarto render failed.", call. = FALSE)
}
