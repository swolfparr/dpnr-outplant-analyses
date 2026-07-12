library(tidyverse)
library(janitor)

# Project root -----------------------------------------------------------------
# This lets the script run after the project folder has been moved to another
# computer. It searches upward from likely starting folders until it finds the
# plot-specific config folder that defines this project, then sets the working
# directory there.
outplant_find_project_root <- function() {
  source_path <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) NA_character_)
  rscript_arg <- commandArgs(trailingOnly = FALSE) %>%
    str_subset("^--file=") %>%
    str_remove("^--file=") %>%
    first(default = NA_character_)
  rscript_path <- tryCatch(normalizePath(rscript_arg, mustWork = TRUE), error = function(e) NA_character_)
  rstudio_path <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch(normalizePath(rstudioapi::getActiveDocumentContext()$path, mustWork = TRUE), error = function(e) NA_character_)
  } else {
    NA_character_
  }

  candidate_dirs <- c(
    dirname(source_path),
    dirname(rscript_path),
    dirname(rstudio_path),
    getwd()
  ) %>%
    discard(is.na) %>%
    unique()

  for (candidate_dir in candidate_dirs) {
    current_dir <- candidate_dir

    repeat {
      if (dir.exists(file.path(current_dir, "config", "outplant_interval_files"))) {
        return(current_dir)
      }

      parent_dir <- dirname(current_dir)
      if (identical(parent_dir, current_dir)) {
        break
      }
      current_dir <- parent_dir
    }
  }

  stop(
    "Could not find the project folder. Open the Coral_survivorship_project folder ",
    "or set your working directory to the folder that contains ",
    "config/outplant_interval_files/.",
    call. = FALSE
  )
}

outplant_project_root <- outplant_find_project_root()
setwd(outplant_project_root)

# Purpose ----------------------------------------------------------------------
# This script is the future-facing workflow for outplant-only TagLab match files.
# Each CSV compares one plot between two survey months. When all months are exported together from TagLab, the numeric Genet column can be used as the long-term tracking ID. A single Genet can still have multiple blob rows in one interval because of split/fuse behavior, so this script collapses to one row per Genet per interval before calculating survivorship, cover, and prevalence.


##### User-editable config ##### ----------------------------------------------------------
# To run this workflow on a new plot or month, add rows to that plot's CSV in
# config/outplant_interval_files/. Keeping one config per plot makes it easy to
# see which TagLab match files have already been entered.
# The script should not need edits as long as the TagLab export has the same
# general columns: Genet, Blob1, Blob2, Area1, Area2, Class, Action, Split/Fuse.
outplant_config_path <- file.path("config", "outplant_interval_files")
outplant_depth_path <- file.path("data_raw", "metadata", "Coordinates_Depths.csv")
outplant_temperature_path <- file.path(
  "data_raw", "Temperature", "2025",
  "21706349 Deep Nurser 2025-11-20 14_38_59 AST (Data AST).csv"
)

outplant_processed_dir <- "data_processed"
outplant_table_dir <- file.path("outputs", "Tables")
outplant_figure_dir <- file.path("outputs", "Figures")

# General helpers ---------------------------------------------------------------
# These functions keep repeated setup, cleaning, and saving code in one place.
outplant_setup_output_dirs <- function() {
  dir.create(outplant_processed_dir, showWarnings = FALSE)
  dir.create(outplant_table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outplant_figure_dir, recursive = TRUE, showWarnings = FALSE)
}

outplant_write_named_csvs <- function(named_tables, directory) {
  iwalk(named_tables, ~ write_csv(.x, file.path(directory, paste0(.y, ".csv"))))
}

outplant_save_named_plots <- function(named_plots, directory, width = 7, height = 4.5, dpi = 300) {
  iwalk(named_plots, ~ ggsave(
    filename = file.path(directory, paste0(.y, ".png")),
    plot = .x,
    width = width,
    height = height,
    dpi = dpi
  ))
}

outplant_clean_text <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    na_if("") %>%
    na_if("NA")
}

outplant_clean_species <- function(x) {
  species <- x %>%
    outplant_clean_text() %>%
    str_replace_all("_", " ")

  case_when(
    species == "Montastrea cavernosa" ~ "Montastraea cavernosa",
    TRUE ~ species
  )
}

outplant_normalize_plot_name <- function(x) {
  x %>%
    outplant_clean_text() %>%
    str_replace("^Plot([A-H])$", "Plot \\1")
}

outplant_section_label <- function(plot, plot_section) {
  if_else(
    is.na(plot_section) | plot_section == "",
    plot,
    str_c(plot, " ", plot_section)
  )
}

outplant_first_non_missing <- function(x) {
  first(na.omit(x), default = NA)
}

outplant_safe_character_col <- function(df, col) {
  if (col %in% names(df)) {
    as.character(df[[col]])
  } else {
    NA_character_
  }
}

outplant_first_matching_col <- function(df, pattern, file) {
  hit <- names(df)[str_detect(names(df), pattern)]

  if (length(hit) == 0) {
    stop("Could not find a column matching '", pattern, "' in ", file, call. = FALSE)
  }

  hit[[1]]
}

outplant_binomial_percent_ci <- function(successes, trials) {
  if (is.na(trials) || trials == 0 || is.na(successes)) {
    return(tibble(percent_survival_low = NA_real_, percent_survival_high = NA_real_))
  }

  ci <- binom.test(successes, trials)$conf.int * 100

  tibble(
    percent_survival_low = ci[[1]],
    percent_survival_high = ci[[2]]
  )
}

outplant_convert_area_to_m2 <- function(area, units) {
  case_when(
    units == "m2" ~ area,
    units == "cm2" ~ area / 10000,
    units == "mm2" ~ area / 1000000,
    TRUE ~ NA_real_
  )
}

outplant_theme <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "grey30", size = base_size),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey15"),
      legend.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.major.x = element_blank()
    )
}

outplant_species_palette <- c(
  "Acropora cervicornis" = "#CC79A7",
  "Acropora palmata" = "#E69F00",
  "Montastraea cavernosa" = "#8A6FDF",
  "Orbicella annularis" = "#009E73",
  "Orbicella faveolata" = "#0072B2",
  "Orbicella franksi" = "#D55E00",
  "Orbicella sp." = "#56B4E9",
  "Porites porites" = "#F0E442"
)

# Config and file readers -------------------------------------------------------
# The config stores file metadata, plot sections, and plot dimensions. Plot
# sections keep TagLab subsections such as Plot E 1-0 and Plot E 1-1 separate.
outplant_read_config <- function(path) {
  config_files <- list.files(
    path,
    pattern = "^Plot_[A-H]\\.csv$",
    full.names = TRUE
  ) %>%
    sort()

  if (length(config_files) == 0) {
    stop(
      "No plot config files were found in ", path,
      ". Expected Plot_A.csv through Plot_H.csv.",
      call. = FALSE
    )
  }

  raw_config <- map_dfr(config_files, function(config_file) {
    read_csv(
      config_file,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    ) %>%
      clean_names() %>%
      mutate(config_file = basename(config_file))
  })

  required_columns <- c(
    "plot", "plot_section", "plot_folder", "file", "month_start",
    "month_end", "month_start_date", "month_end_date", "file_type",
    "match_type", "plot_length_m", "plot_width_m", "taglab_area_units"
  )
  missing_columns <- setdiff(required_columns, names(raw_config))

  if (length(missing_columns) > 0) {
    stop(
      "Plot config files are missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"plot_section" %in% names(raw_config)) {
    raw_config <- raw_config %>% mutate(plot_section = match_type)
  }

  raw_config %>%
    filter(!is.na(file), str_squish(file) != "") %>%
    clean_names() %>%
    mutate(
      plot = outplant_normalize_plot_name(plot),
      plot_section = outplant_clean_text(plot_section),
      plot_section_label = outplant_section_label(plot, plot_section),
      plot_folder = str_squish(plot_folder),
      file_type = str_to_lower(file_type),
      match_type = as.character(match_type),
      taglab_area_units = str_to_lower(taglab_area_units),
      plot_length_m = parse_double(plot_length_m),
      plot_width_m = parse_double(plot_width_m),
      month_start_date = as.Date(month_start_date),
      month_end_date = as.Date(month_end_date),
      plot_area_m2 = plot_length_m * plot_width_m
    ) %>%
    arrange(factor(plot, levels = paste("Plot", LETTERS[1:8])), plot_section, month_start_date) %>%
    group_by(plot, plot_section) %>%
    mutate(interval_order = row_number()) %>%
    ungroup()
}

outplant_build_file_audit <- function(config) {
  config %>%
    mutate(
      path = file.path("data_raw", "taglab", plot_folder, file),
      file_exists = file.exists(path)
    ) %>%
    select(
      plot, plot_section, plot_section_label, month_start, month_end,
      month_start_date, month_end_date, match_type, file, path, file_exists
    )
}

outplant_read_taglab_file <- function(plot_folder, file, file_type) {
  path <- file.path("data_raw", "taglab", plot_folder, file)

  if (!file.exists(path)) {
    stop("Could not find outplant TagLab file: ", path, call. = FALSE)
  }

  if (file_type == "csv") {
    read_csv(path, show_col_types = FALSE, name_repair = "unique")
  } else if (file_type %in% c("xls", "xlsx")) {
    readxl::read_xlsx(path, .name_repair = "unique")
  } else {
    stop("Unsupported file_type '", file_type, "' for ", file, call. = FALSE)
  }
}

# Standardization ---------------------------------------------------------------
# This turns every TagLab match export into the same column names, even if the original file says Area1 (May), Area1 (June), Area1, etc.
outplant_standardize_interval_file <- function(plot, plot_section, plot_section_label,
                                               plot_folder, file, month_start, month_end,
                                               month_start_date, month_end_date, file_type,
                                               match_type, plot_length_m, plot_width_m,
                                               taglab_area_units, plot_area_m2, interval_order) {
  df <- outplant_read_taglab_file(plot_folder, file, file_type) %>%
    clean_names()

  area_start_col <- outplant_first_matching_col(df, "^area1", file)
  area_end_col <- outplant_first_matching_col(df, "^area2", file)
  blob_start_col <- outplant_first_matching_col(df, "^blob1", file)
  blob_end_col <- outplant_first_matching_col(df, "^blob2", file)

  df %>%
    transmute(
      plot = .env$plot,
      plot_section = .env$plot_section,
      plot_section_label = .env$plot_section_label,
      source_file = .env$file,
      match_type = .env$match_type,
      interval_order = .env$interval_order,
      month_start = .env$month_start,
      month_end = .env$month_end,
      month_start_date = .env$month_start_date,
      month_end_date = .env$month_end_date,
      plot_length_m = .env$plot_length_m,
      plot_width_m = .env$plot_width_m,
      plot_area_m2 = .env$plot_area_m2,
      taglab_area_units = .env$taglab_area_units,
      interval_row_id = row_number(),
      genet = outplant_clean_text(genet),
      blob_start = outplant_clean_text(.data[[blob_start_col]]),
      blob_end = outplant_clean_text(.data[[blob_end_col]]),
      area_start = as.numeric(.data[[area_start_col]]),
      area_end = as.numeric(.data[[area_end_col]]),
      species = outplant_clean_species(class),
      action = str_to_lower(outplant_clean_text(action)),
      split_fuse = str_to_lower(outplant_clean_text(split_fuse)),
      tag = outplant_clean_text(outplant_safe_character_col(df, "tag")),
      cluster_id = coalesce(
        outplant_clean_text(outplant_safe_character_col(df, "cluster_id")),
        outplant_clean_text(outplant_safe_character_col(df, "cluster"))
      ),
      genotype = outplant_clean_text(outplant_safe_character_col(df, "genotype")),
      present_start = !is.na(area_start) & area_start > 0,
      present_end = !is.na(area_end) & area_end > 0,
      survived_interval = present_start & present_end,
      died_interval = present_start & !present_end,
      born_interval = !present_start & present_end,
      area_start_m2 = outplant_convert_area_to_m2(area_start, taglab_area_units),
      area_end_m2 = outplant_convert_area_to_m2(area_end, taglab_area_units),
      percent_area_change = case_when(
        present_start & present_end & area_start > 0 ~ 100 * (area_end - area_start) / area_start,
        TRUE ~ NA_real_
      )
    )
}

outplant_build_interval_rows <- function(config) {
  config %>%
    select(
      plot, plot_section, plot_section_label, plot_folder, file, month_start,
      month_end, month_start_date, month_end_date, file_type, match_type,
      plot_length_m, plot_width_m, taglab_area_units, plot_area_m2,
      interval_order
    ) %>%
    pmap_dfr(outplant_standardize_interval_file)
}

# Coral-level interval summaries ------------------------------------------------
outplant_sum_unique_blob_area <- function(blob, area) {
  keep <- !is.na(area) & area > 0

  if (!any(keep)) {
    return(0)
  }

  blob_key <- as.character(blob)
  missing_blob <- is.na(blob_key) | blob_key == "" | blob_key == "-1"
  blob_key[missing_blob] <- str_c("row_", seq_along(blob_key))[missing_blob]

  sum(vapply(split(area[keep], blob_key[keep]), max, numeric(1), na.rm = TRUE), na.rm = TRUE)
}

# Some files can contain repeated rows for the same TagLab Genet because of
# split/fuse events or multiple blob pieces. Grouping by Genet makes one row per tracked outplant per interval before calculating survivorship.
outplant_build_coral_intervals <- function(interval_rows) {
  interval_rows %>%
    filter(!is.na(genet)) %>%
    group_by(plot, plot_section, plot_section_label, interval_order, month_start, month_end, genet) %>%
    summarise(
      tag = outplant_first_non_missing(tag),
      genotype = outplant_first_non_missing(genotype),
      species = outplant_first_non_missing(species),
      month_start_date = first(month_start_date),
      month_end_date = first(month_end_date),
      source_files = str_c(sort(unique(source_file)), collapse = "; "),
      match_types = str_c(sort(unique(match_type)), collapse = "; "),
      actions = str_c(sort(unique(na.omit(action))), collapse = "; "),
      split_fuse_events = str_c(sort(unique(na.omit(split_fuse))), collapse = "; "),
      n_rows = n(),
      plot_area_m2 = first(plot_area_m2),
      taglab_area_units = first(taglab_area_units),
      area_start = outplant_sum_unique_blob_area(blob_start, area_start),
      area_end = outplant_sum_unique_blob_area(blob_end, area_end),
      area_start_m2 = outplant_sum_unique_blob_area(blob_start, area_start_m2),
      area_end_m2 = outplant_sum_unique_blob_area(blob_end, area_end_m2),
      present_start = any(present_start, na.rm = TRUE),
      present_end = any(present_end, na.rm = TRUE),
      survived_interval = present_start & present_end,
      died_interval = present_start & !present_end,
      born_interval = !present_start & present_end,
      .groups = "drop"
    )
}

outplant_combine_plot_sections <- function(section_coral_intervals) {
  section_coral_intervals %>%
    mutate(
      source_plot_section = plot_section,
      source_plot_section_label = plot_section_label,
      genet = str_c(plot_section, genet, sep = "__"),
      plot_section = "all",
      plot_section_label = plot
    )
}

# Month-level observations ------------------------------------------------------
# Cover and species prevalence are month-level questions. This uses one snapshot
# per month: the start side of the first interval for the baseline month, and the end side of each interval for all later months. That avoids double-counting the same survey month from two adjacent files.

outplant_build_monthly_observations <- function(coral_intervals) {
  first_interval_by_plot <- coral_intervals %>%
    group_by(plot, plot_section, plot_section_label) %>%
    summarise(first_interval_order = min(interval_order, na.rm = TRUE), .groups = "drop")

  baseline_obs <- coral_intervals %>%
    inner_join(first_interval_by_plot, by = c("plot", "plot_section", "plot_section_label")) %>%
    filter(interval_order == first_interval_order) %>%
    transmute(
      plot,
      plot_section,
      plot_section_label,
      month = month_start,
      month_order = interval_order,
      survey_date = month_start_date,
      observation_source = "baseline_start",
      genet,
      tag,
      genotype,
      species,
      plot_area_m2,
      taglab_area_units,
      area = area_start,
      area_m2 = area_start_m2,
      present = present_start
    )

  end_obs <- coral_intervals %>%
    transmute(
      plot,
      plot_section,
      plot_section_label,
      month = month_end,
      month_order = interval_order + 1,
      survey_date = month_end_date,
      observation_source = "interval_end",
      genet,
      tag,
      genotype,
      species,
      plot_area_m2,
      taglab_area_units,
      area = area_end,
      area_m2 = area_end_m2,
      present = present_end
    )

  bind_rows(baseline_obs, end_obs) %>%
    mutate(
      area = if_else(is.infinite(area), NA_real_, area),
      area_m2 = if_else(is.infinite(area_m2), NA_real_, area_m2),
      snapshot_object_id = str_c(plot_section_label, month_order, observation_source, genet, sep = "__"),
      n_duplicate_observations = 1L
    ) %>%
    arrange(plot, plot_section, month_order, genet)
}

# Analysis summaries ------------------------------------------------------------
# Cumulative survivorship is baseline-based: the first survey month is 100%, and later months ask how many baseline Genets are still present. Once a baseline Genet is absent, it remains counted as dead for later months.
outplant_summarize_cumulative_survival <- function(monthly_observations, coral_intervals) {
  baseline_month_lookup <- monthly_observations %>%
    filter(present) %>%
    group_by(plot, plot_section, plot_section_label) %>%
    slice_min(month_order, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      plot,
      plot_section,
      plot_section_label,
      baseline_month_order = month_order,
      baseline_month = month,
      baseline_date = survey_date
    )

  baseline_genets <- monthly_observations %>%
    inner_join(
      baseline_month_lookup,
      by = c("plot", "plot_section", "plot_section_label")
    ) %>%
    filter(month_order == baseline_month_order, present) %>%
    select(
      plot, plot_section, plot_section_label, baseline_month_order,
      baseline_month, baseline_date, genet, species
    )

  month_lookup <- monthly_observations %>%
    distinct(plot, plot_section, plot_section_label, month_order, month, survey_date) %>%
    inner_join(
      baseline_month_lookup %>%
        select(plot, plot_section, plot_section_label, baseline_month_order),
      by = c("plot", "plot_section", "plot_section_label")
    ) %>%
    filter(month_order >= baseline_month_order) %>%
    arrange(plot, plot_section, month_order)

  baseline_by_plot <- baseline_genets %>%
    group_by(plot, plot_section, plot_section_label) %>%
    summarise(
      baseline_month = first(baseline_month),
      baseline_date = first(baseline_date),
      n_baseline = n_distinct(genet),
      .groups = "drop"
    )

  baseline_tracking <- baseline_genets %>%
    select(plot, plot_section, plot_section_label, genet, species) %>%
    inner_join(month_lookup, by = c("plot", "plot_section", "plot_section_label"), relationship = "many-to-many") %>%
    left_join(
      monthly_observations %>% select(plot, plot_section, month_order, month, survey_date, genet, present, area_m2),
      by = c("plot", "plot_section", "month_order", "month", "survey_date", "genet")
    ) %>%
    mutate(
      present = replace_na(present, FALSE),
      area_m2 = replace_na(area_m2, 0)
    ) %>%
    arrange(plot, plot_section, genet, month_order) %>%
    group_by(plot, plot_section, genet) %>%
    mutate(
      surviving_from_baseline = cumall(present),
      died_this_interval = lag(surviving_from_baseline, default = TRUE) & !surviving_from_baseline,
      area_m2 = if_else(surviving_from_baseline, area_m2, 0)
    ) %>%
    ungroup()

  baseline_tracking %>%
    group_by(plot, plot_section, plot_section_label, month_order, month, survey_date) %>%
    summarise(
      n_surviving_from_baseline = sum(surviving_from_baseline, na.rm = TRUE),
      n_died_interval = sum(died_this_interval, na.rm = TRUE),
      baseline_surviving_area_m2 = sum(area_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(plot, plot_section) %>%
    mutate(cumulative_deaths_observed = cumsum(n_died_interval)) %>%
    ungroup() %>%
    left_join(baseline_by_plot, by = c("plot", "plot_section", "plot_section_label")) %>%
    mutate(
      percent_cumulative_survival = 100 * n_surviving_from_baseline / n_baseline,
      ci = map2(n_surviving_from_baseline, n_baseline, outplant_binomial_percent_ci)
    ) %>%
    unnest_wider(ci) %>%
    rename(
      percent_cumulative_survival_low = percent_survival_low,
      percent_cumulative_survival_high = percent_survival_high
    ) %>%
    arrange(plot, plot_section, month_order)
}

outplant_summarize_cumulative_survival_by_plot <- function(section_cumulative_survival) {
  complete_plot_months <- section_cumulative_survival %>%
    distinct(plot, plot_section, plot_section_label) %>%
    group_by(plot) %>%
    summarise(
      expected_plot_sections = n_distinct(plot_section),
      expected_plot_section_labels = str_c(sort(unique(plot_section_label)), collapse = "; "),
      .groups = "drop"
    )

  section_cumulative_survival %>%
    group_by(plot, month_order, month, survey_date) %>%
    summarise(
      n_plot_sections = n_distinct(plot_section),
      plot_section_labels = str_c(sort(unique(plot_section_label)), collapse = "; "),
      plot_section = "all",
      plot_section_label = first(plot),
      baseline_month = str_c(sort(unique(baseline_month)), collapse = "; "),
      baseline_date = min(baseline_date, na.rm = TRUE),
      n_baseline = sum(n_baseline, na.rm = TRUE),
      n_died_interval = sum(n_died_interval, na.rm = TRUE),
      cumulative_deaths_observed = sum(cumulative_deaths_observed, na.rm = TRUE),
      n_surviving_from_baseline = sum(n_surviving_from_baseline, na.rm = TRUE),
      baseline_surviving_area_m2 = sum(baseline_surviving_area_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(complete_plot_months, by = "plot") %>%
    filter(n_plot_sections == expected_plot_sections) %>%
    mutate(
      percent_cumulative_survival = 100 * n_surviving_from_baseline / n_baseline,
      ci = map2(n_surviving_from_baseline, n_baseline, outplant_binomial_percent_ci)
    ) %>%
    unnest_wider(ci) %>%
    rename(
      percent_cumulative_survival_low = percent_survival_low,
      percent_cumulative_survival_high = percent_survival_high
    ) %>%
    arrange(plot, month_order)
}

outplant_summarize_survival <- function(coral_intervals) {
  coral_intervals %>%
    group_by(plot, plot_section, plot_section_label, interval_order, month_start, month_end) %>%
    summarise(
      n_start = sum(present_start, na.rm = TRUE),
      n_survived = sum(survived_interval, na.rm = TRUE),
      n_died = sum(died_interval, na.rm = TRUE),
      n_born = sum(born_interval, na.rm = TRUE),
      percent_survival = 100 * n_survived / n_start,
      .groups = "drop"
    ) %>%
    mutate(ci = map2(n_survived, n_start, outplant_binomial_percent_ci)) %>%
    unnest_wider(ci) %>%
    mutate(interval_label = str_c(month_start, " to ", month_end)) %>%
    arrange(plot, plot_section, interval_order)
}

outplant_summarize_survival_by_plot <- function(section_survival) {
  section_survival %>%
    group_by(plot, interval_order, month_start, month_end, interval_label) %>%
    summarise(
      plot_section = "all",
      plot_section_label = first(plot),
      n_start = sum(n_start, na.rm = TRUE),
      n_survived = sum(n_survived, na.rm = TRUE),
      n_died = sum(n_died, na.rm = TRUE),
      n_born = sum(n_born, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percent_survival = 100 * n_survived / n_start,
      ci = map2(n_survived, n_start, outplant_binomial_percent_ci)
    ) %>%
    unnest_wider(ci) %>%
    arrange(plot, interval_order)
}

outplant_summarize_species_survival <- function(coral_intervals) {
  coral_intervals %>%
    filter(!is.na(species)) %>%
    group_by(plot, plot_section, plot_section_label, interval_order, month_start, month_end, species) %>%
    summarise(
      n_start = sum(present_start, na.rm = TRUE),
      n_survived = sum(survived_interval, na.rm = TRUE),
      n_died = sum(died_interval, na.rm = TRUE),
      percent_survival = 100 * n_survived / n_start,
      .groups = "drop"
    ) %>%
    mutate(ci = map2(n_survived, n_start, outplant_binomial_percent_ci)) %>%
    unnest_wider(ci) %>%
    mutate(interval_label = str_c(month_start, " to ", month_end)) %>%
    arrange(plot, plot_section, interval_order, species)
}

outplant_summarize_species_survival_by_plot <- function(section_species_survival) {
  section_species_survival %>%
    group_by(plot, interval_order, month_start, month_end, interval_label, species) %>%
    summarise(
      plot_section = "all",
      plot_section_label = first(plot),
      n_start = sum(n_start, na.rm = TRUE),
      n_survived = sum(n_survived, na.rm = TRUE),
      n_died = sum(n_died, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percent_survival = 100 * n_survived / n_start,
      ci = map2(n_survived, n_start, outplant_binomial_percent_ci)
    ) %>%
    unnest_wider(ci) %>%
    arrange(plot, interval_order, species)
}

outplant_summarize_cover <- function(monthly_observations) {
  monthly_observations %>%
    filter(present) %>%
    group_by(plot, plot_section, plot_section_label, month_order, month) %>%
    summarise(
      n_outplants_present = n(),
      total_outplant_area_m2 = sum(area_m2, na.rm = TRUE),
      plot_area_m2 = first(plot_area_m2),
      percent_outplant_cover = 100 * total_outplant_area_m2 / plot_area_m2,
      .groups = "drop"
    ) %>%
    arrange(plot, plot_section, month_order)
}

outplant_summarize_species_prevalence <- function(monthly_observations) {
  monthly_observations %>%
    filter(present, !is.na(species)) %>%
    group_by(plot, plot_section, plot_section_label, month_order, month, species) %>%
    summarise(
      n_outplants = n(),
      total_outplant_area_m2 = sum(area_m2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(plot, plot_section, plot_section_label, month_order, month) %>%
    mutate(
      total_outplants_month = sum(n_outplants),
      percent_prevalence = 100 * n_outplants / total_outplants_month,
      total_area_month_m2 = sum(total_outplant_area_m2),
      percent_cover_within_outplants = 100 * total_outplant_area_m2 / total_area_month_m2
    ) %>%
    ungroup() %>%
    arrange(plot, plot_section, month_order, species)
}

outplant_read_depth_metadata <- function(path) {
  if (!file.exists(path)) {
    warning("Could not find plot depth metadata file: ", path, call. = FALSE)
    return(tibble())
  }

  read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>%
    transmute(
      source_plot_id = outplant_clean_text(id),
      plot = outplant_normalize_plot_name(id_new),
      marker = outplant_clean_text(marker),
      latitude = as.numeric(lat),
      longitude = as.numeric(lon),
      depth_ft = as.numeric(depth_ft),
      depth_m = as.numeric(depth_m)
    ) %>%
    filter(!is.na(plot), !is.na(marker))
}

outplant_summarize_plot_depth <- function(depth_metadata) {
  if (nrow(depth_metadata) == 0) {
    return(tibble())
  }

  depth_metadata %>%
    group_by(plot) %>%
    summarise(
      n_depth_markers = n(),
      mean_depth_ft = mean(depth_ft, na.rm = TRUE),
      min_depth_ft = min(depth_ft, na.rm = TRUE),
      max_depth_ft = max(depth_ft, na.rm = TRUE),
      depth_range_ft = max_depth_ft - min_depth_ft,
      mean_depth_m = mean(depth_m, na.rm = TRUE),
      min_depth_m = min(depth_m, na.rm = TRUE),
      max_depth_m = max(depth_m, na.rm = TRUE),
      depth_range_m = max_depth_m - min_depth_m,
      centroid_latitude = mean(latitude, na.rm = TRUE),
      centroid_longitude = mean(longitude, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(plot)
}

outplant_summarize_species_depth <- function(species_prevalence, plot_depth_summary) {
  if (nrow(species_prevalence) == 0 || nrow(plot_depth_summary) == 0) {
    return(tibble())
  }

  species_prevalence %>%
    left_join(plot_depth_summary, by = "plot") %>%
    select(
      plot, plot_section, plot_section_label, month_order, month, species,
      n_outplants, percent_prevalence, total_outplant_area_m2,
      percent_cover_within_outplants,
      mean_depth_ft, min_depth_ft, max_depth_ft, depth_range_ft,
      mean_depth_m, min_depth_m, max_depth_m, depth_range_m
    ) %>%
    arrange(plot, plot_section, month_order, species)
}

outplant_read_temperature_file <- function(path) {
  if (!file.exists(path)) {
    warning("Could not find temperature file: ", path, call. = FALSE)
    return(tibble())
  }

  extension <- str_to_lower(tools::file_ext(path))
  raw_temperature <- if (extension == "csv") {
    read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  } else if (extension %in% c("xls", "xlsx")) {
    readxl::read_xlsx(path, .name_repair = "unique")
  } else {
    stop("Unsupported temperature file type: ", path, call. = FALSE)
  }

  date_col <- names(raw_temperature)[str_detect(names(raw_temperature), regex("date-time", ignore_case = TRUE))][1]
  temp_col <- names(raw_temperature)[str_detect(names(raw_temperature), regex("temperature", ignore_case = TRUE))][1]

  if (is.na(date_col) || is.na(temp_col)) {
    stop("Temperature file must include Date-Time and Temperature columns: ", path, call. = FALSE)
  }

  date_time <- raw_temperature[[date_col]]
  if (!inherits(date_time, "POSIXt")) {
    date_time <- mdy_hms(date_time, tz = "America/St_Thomas", quiet = TRUE)
  }

  tibble(
    source_file = basename(path),
    logger_location = "Deep Nursery",
    date_time = date_time,
    date = as.Date(date_time),
    temperature_c = readr::parse_number(as.character(raw_temperature[[temp_col]]))
  ) %>%
    filter(!is.na(date_time), !is.na(temperature_c))
}

outplant_summarize_daily_temperature <- function(temperature_observations, cumulative_survival_summary) {
  if (nrow(temperature_observations) == 0 || !"survey_date" %in% names(cumulative_survival_summary)) {
    return(tibble())
  }

  survey_start <- min(cumulative_survival_summary$survey_date, na.rm = TRUE)
  survey_end <- max(cumulative_survival_summary$survey_date, na.rm = TRUE)

  temperature_observations %>%
    filter(date >= survey_start, date <= survey_end) %>%
    group_by(logger_location, date) %>%
    summarise(
      n_temperature_records = n(),
      mean_temperature_c = mean(temperature_c, na.rm = TRUE),
      min_temperature_c = min(temperature_c, na.rm = TRUE),
      max_temperature_c = max(temperature_c, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)
}

outplant_build_master_tracking_dataset <- function(monthly_observations) {
  baseline_month_lookup <- monthly_observations %>%
    filter(present) %>%
    group_by(plot, plot_section, plot_section_label) %>%
    slice_min(month_order, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      plot,
      plot_section,
      plot_section_label,
      baseline_month_order = month_order
    )

  month_lookup <- monthly_observations %>%
    distinct(plot, plot_section, plot_section_label, month_order, month, survey_date) %>%
    inner_join(
      baseline_month_lookup,
      by = c("plot", "plot_section", "plot_section_label")
    ) %>%
    filter(month_order >= baseline_month_order) %>%
    arrange(plot, plot_section, month_order)

  baseline_genets <- monthly_observations %>%
    inner_join(
      baseline_month_lookup,
      by = c("plot", "plot_section", "plot_section_label")
    ) %>%
    filter(month_order == baseline_month_order, present) %>%
    select(plot, plot_section, plot_section_label, genet, baseline_species = species)

  baseline_tracking <- baseline_genets %>%
    inner_join(month_lookup, by = c("plot", "plot_section", "plot_section_label"), relationship = "many-to-many") %>%
    left_join(
      monthly_observations %>%
        select(
          plot, plot_section, month_order, month, genet, observation_source, species,
          survey_date, plot_area_m2, taglab_area_units, area, area_m2, present
        ),
      by = c("plot", "plot_section", "month_order", "month", "survey_date", "genet")
    ) %>%
    mutate(
      is_baseline_genet = TRUE,
      species = coalesce(species, baseline_species),
      present = replace_na(present, FALSE),
      area = replace_na(area, 0),
      area_m2 = replace_na(area_m2, 0)
    ) %>%
    arrange(plot, plot_section, genet, month_order) %>%
    group_by(plot, plot_section, genet) %>%
    mutate(
      surviving_from_baseline = cumall(present),
      died_from_baseline_this_month = lag(surviving_from_baseline, default = TRUE) & !surviving_from_baseline
    ) %>%
    ungroup()

  non_baseline_tracking <- monthly_observations %>%
    anti_join(baseline_genets, by = c("plot", "plot_section", "genet")) %>%
    mutate(
      baseline_species = NA_character_,
      is_baseline_genet = FALSE,
      surviving_from_baseline = NA,
      died_from_baseline_this_month = FALSE
    )

  bind_rows(baseline_tracking, non_baseline_tracking) %>%
    mutate(
      percent_cover_of_plot = 100 * area_m2 / plot_area_m2
    ) %>%
    select(
      plot, plot_section, plot_section_label, month_order, month, survey_date,
      observation_source, genet, species, is_baseline_genet, present,
      surviving_from_baseline,
      died_from_baseline_this_month, area, area_m2, plot_area_m2,
      percent_cover_of_plot, taglab_area_units
    ) %>%
    arrange(plot, plot_section, month_order, genet)
}

outplant_build_master_summary_dataset <- function(cumulative_survival, interval_survival, species_interval_survival, cover_summary, species_prevalence) {
  bind_rows(
    cumulative_survival %>%
      transmute(
        summary_level = "plot_month",
        plot,
        plot_section,
        plot_section_label,
        time_label = month,
        month_order,
        month,
        interval_order = NA_integer_,
        month_start = NA_character_,
        month_end = NA_character_,
        species = NA_character_,
        metric = "cumulative_survivorship",
        value = percent_cumulative_survival,
        units = "percent",
        n = n_surviving_from_baseline,
        denominator = n_baseline,
        ci_low = percent_cumulative_survival_low,
        ci_high = percent_cumulative_survival_high,
        notes = str_c("Baseline month: ", baseline_month)
      ),
    cumulative_survival %>%
      transmute(
        summary_level = "plot_month",
        plot,
        plot_section,
        plot_section_label,
        time_label = month,
        month_order,
        month,
        interval_order = NA_integer_,
        month_start = NA_character_,
        month_end = NA_character_,
        species = NA_character_,
        metric = "cumulative_deaths",
        value = cumulative_deaths_observed,
        units = "count",
        n = cumulative_deaths_observed,
        denominator = n_baseline,
        ci_low = NA_real_,
        ci_high = NA_real_,
        notes = str_c("Baseline month: ", baseline_month)
      ),
    cover_summary %>%
      transmute(
        summary_level = "plot_month",
        plot,
        plot_section,
        plot_section_label,
        time_label = month,
        month_order,
        month,
        interval_order = NA_integer_,
        month_start = NA_character_,
        month_end = NA_character_,
        species = NA_character_,
        metric = "outplant_cover",
        value = percent_outplant_cover,
        units = "percent_of_plot",
        n = n_outplants_present,
        denominator = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        notes = str_c("Total area m2: ", round(total_outplant_area_m2, 6))
      ),
    interval_survival %>%
      transmute(
        summary_level = "plot_interval",
        plot,
        plot_section,
        plot_section_label,
        time_label = interval_label,
        month_order = NA_real_,
        month = NA_character_,
        interval_order,
        month_start,
        month_end,
        species = NA_character_,
        metric = "interval_survivorship",
        value = percent_survival,
        units = "percent",
        n = n_survived,
        denominator = n_start,
        ci_low = percent_survival_low,
        ci_high = percent_survival_high,
        notes = str_c("Deaths: ", n_died, "; births/new detections: ", n_born)
      ),
    species_interval_survival %>%
      transmute(
        summary_level = "species_interval",
        plot,
        plot_section,
        plot_section_label,
        time_label = interval_label,
        month_order = NA_real_,
        month = NA_character_,
        interval_order,
        month_start,
        month_end,
        species,
        metric = "species_interval_survivorship",
        value = percent_survival,
        units = "percent",
        n = n_survived,
        denominator = n_start,
        ci_low = percent_survival_low,
        ci_high = percent_survival_high,
        notes = str_c("Deaths: ", n_died)
      ),
    species_prevalence %>%
      transmute(
        summary_level = "species_month",
        plot,
        plot_section,
        plot_section_label,
        time_label = month,
        month_order,
        month,
        interval_order = NA_integer_,
        month_start = NA_character_,
        month_end = NA_character_,
        species,
        metric = "species_prevalence",
        value = percent_prevalence,
        units = "percent_of_present_outplants",
        n = n_outplants,
        denominator = total_outplants_month,
        ci_low = NA_real_,
        ci_high = NA_real_,
        notes = str_c("Species area m2: ", round(total_outplant_area_m2, 6))
      ),
    species_prevalence %>%
      transmute(
        summary_level = "species_month",
        plot,
        plot_section,
        plot_section_label,
        time_label = month,
        month_order,
        month,
        interval_order = NA_integer_,
        month_start = NA_character_,
        month_end = NA_character_,
        species,
        metric = "species_cover_share",
        value = percent_cover_within_outplants,
        units = "percent_of_outplant_cover",
        n = n_outplants,
        denominator = total_outplants_month,
        ci_low = NA_real_,
        ci_high = NA_real_,
        notes = str_c("Species area m2: ", round(total_outplant_area_m2, 6))
      )
  ) %>%
    arrange(plot, plot_section, summary_level, metric, month_order, interval_order, species)
}

outplant_build_qa <- function(interval_rows, monthly_observations, coral_intervals, file_audit) {
  shared_month_end <- coral_intervals %>%
    filter(present_end) %>%
    transmute(
      plot,
      plot_section,
      plot_section_label,
      shared_month_order = interval_order + 1,
      shared_month = month_end,
      genet,
      species_end = species,
      area_end_snapshot = area_end
    )

  shared_month_start <- coral_intervals %>%
    filter(present_start) %>%
    transmute(
      plot,
      plot_section,
      plot_section_label,
      shared_month_order = interval_order,
      shared_month = month_start,
      genet,
      species_start = species,
      area_start_snapshot = area_start
    )

  shared_month_continuity <- full_join(
    shared_month_end,
    shared_month_start,
    by = c("plot", "plot_section", "plot_section_label", "shared_month_order", "shared_month", "genet")
  ) %>%
    group_by(plot, plot_section, plot_section_label, shared_month_order, shared_month) %>%
    summarise(
      has_previous_end = any(!is.na(area_end_snapshot)),
      has_next_start = any(!is.na(area_start_snapshot)),
      previous_end_n = sum(!is.na(area_end_snapshot)),
      next_start_n = sum(!is.na(area_start_snapshot)),
      in_both_snapshots = sum(!is.na(area_end_snapshot) & !is.na(area_start_snapshot)),
      missing_from_next_start = sum(!is.na(area_end_snapshot) & is.na(area_start_snapshot)),
      new_in_next_start = sum(is.na(area_end_snapshot) & !is.na(area_start_snapshot)),
      species_mismatches = sum(
        !is.na(area_end_snapshot) &
          !is.na(area_start_snapshot) &
          species_end != species_start,
        na.rm = TRUE
      ),
      area_mismatches = sum(
        !is.na(area_end_snapshot) &
          !is.na(area_start_snapshot) &
          abs(area_end_snapshot - area_start_snapshot) > 1e-9,
        na.rm = TRUE
      ),
      max_abs_area_difference = if (
        any(!is.na(area_end_snapshot) & !is.na(area_start_snapshot))
      ) {
        max(abs(area_end_snapshot - area_start_snapshot), na.rm = TRUE)
      } else {
        0
      },
      .groups = "drop"
    ) %>%
    filter(has_previous_end, has_next_start) %>%
    select(-has_previous_end, -has_next_start)

  list(
    outplant_file_audit = file_audit,
    outplant_interval_qa = interval_rows %>%
      group_by(plot, plot_section, plot_section_label, interval_order, month_start, month_end, source_file) %>%
      summarise(
        n_rows = n(),
        n_missing_genet = sum(is.na(genet)),
        n_missing_species = sum(is.na(species)),
        n_missing_tag = sum(is.na(tag)),
        n_negative_area_start = sum(area_start < 0, na.rm = TRUE),
        n_negative_area_end = sum(area_end < 0, na.rm = TRUE),
        actions = str_c(sort(unique(na.omit(action))), collapse = "; "),
        split_fuse_events = str_c(sort(unique(na.omit(split_fuse))), collapse = "; "),
        .groups = "drop"
      ),
    outplant_month_duplicate_qa = monthly_observations %>%
      filter(n_duplicate_observations > 1) %>%
      select(plot, plot_section, plot_section_label, month, genet, species, n_duplicate_observations, area, area_m2, present),
    outplant_shared_month_continuity_qa = shared_month_continuity,
    outplant_genet_consistency_qa = interval_rows %>%
      filter(!is.na(genet)) %>%
      group_by(plot, plot_section, plot_section_label, genet) %>%
      summarise(
        n_species = n_distinct(species, na.rm = TRUE),
        species_values = str_c(sort(unique(na.omit(species))), collapse = "; "),
        n_tags = n_distinct(tag, na.rm = TRUE),
        tag_values = str_c(sort(unique(na.omit(tag))), collapse = "; "),
        n_genotypes = n_distinct(genotype, na.rm = TRUE),
        genotype_values = str_c(sort(unique(na.omit(genotype))), collapse = "; "),
        .groups = "drop"
      ) %>%
      filter(n_species > 1 | n_tags > 1 | n_genotypes > 1),
    outplant_unstable_genet_examples = interval_rows %>%
      filter(!is.na(genet)) %>%
      group_by(plot, plot_section, plot_section_label, genet) %>%
      mutate(
        n_species = n_distinct(species, na.rm = TRUE),
        n_tags = n_distinct(tag, na.rm = TRUE),
        n_genotypes = n_distinct(genotype, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(n_species > 1 | n_tags > 1 | n_genotypes > 1) %>%
      select(
        plot, plot_section, plot_section_label, source_file, month_start,
        month_end, genet, tag, genotype, species, action, area_start, area_end
      ) %>%
      arrange(plot, plot_section, genet, month_start, source_file)
  )
}

# Plot builders -----------------------------------------------------------------
# These figures are designed for reports: minimal styling, labels that show the units, and no hidden denominator changes.
outplant_plot_cumulative_survival <- function(cumulative_survival_summary, daily_temperature = tibble()) {
  survival_plot_data <- cumulative_survival_summary %>%
    mutate(survey_date = as.Date(survey_date))

  p <- ggplot(
    survival_plot_data,
    aes(x = survey_date, y = percent_cumulative_survival, group = plot_section_label)
  )

  has_temperature <- nrow(daily_temperature) > 0

  if (has_temperature) {
    temp_min <- floor(min(daily_temperature$mean_temperature_c, na.rm = TRUE))
    temp_max <- ceiling(max(daily_temperature$mean_temperature_c, na.rm = TRUE))

    if (temp_min == temp_max) {
      temp_min <- temp_min - 1
      temp_max <- temp_max + 1
    }

    daily_temperature <- daily_temperature %>%
      mutate(temperature_scaled = 100 * (mean_temperature_c - temp_min) / (temp_max - temp_min))

    p <- p +
      geom_line(
        data = daily_temperature,
        aes(x = date, y = temperature_scaled),
        inherit.aes = FALSE,
        color = "#C43C39",
        linewidth = 0.55,
        alpha = 0.85
      )
  }

  p <- p +
    geom_errorbar(
      aes(
        ymin = percent_cumulative_survival_low,
        ymax = percent_cumulative_survival_high
      ),
      width = 3,
      color = "grey25",
      linewidth = 0.5
    ) +
    geom_line(aes(color = plot_section_label), linewidth = 0.8) +
    geom_point(aes(color = plot_section_label), size = 2.8) +
    geom_text(
      aes(label = sprintf("%.1f%%", percent_cumulative_survival)),
      vjust = -1,
      size = 3.4
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %d",
      expand = expansion(mult = c(0.04, 0.08))
    ) +
    labs(
      x = "Survey date",
      y = "Cumulative survivorship",
      color = "Plot",
      title = "Outplant cumulative survivorship and temperature through time"
    ) +
    scale_color_brewer(palette = "Dark2")

  if (has_temperature) {
    p <- p +
      scale_y_continuous(
        limits = c(0, 100),
        labels = scales::label_percent(scale = 1),
        expand = expansion(mult = c(0.02, 0.08)),
        sec.axis = sec_axis(
          transform = ~ . * (temp_max - temp_min) / 100 + temp_min,
          name = "Daily mean temperature (°C)"
        )
      ) +
      labs(
        subtitle = str_c(
          "Plots shown separately; red line = Deep Nursery daily mean temperature"
        )
      )
  } else {
    p <- p +
      scale_y_continuous(
        limits = c(0, 100),
        labels = scales::label_percent(scale = 1),
        expand = expansion(mult = c(0.02, 0.08))
      ) +
      labs(
        subtitle = "Percent of baseline Genets still present after collapsing repeated blob rows; bars are exact binomial 95% CIs"
      )
  }

  p +
    outplant_theme() +
    theme(
      axis.text.x = element_text(angle = 25, hjust = 1),
      axis.title.y.right = element_text(color = "#C43C39"),
      axis.text.y.right = element_text(color = "#C43C39")
    )
}

outplant_add_monitoring_labels <- function(summary_data) {
  summary_data %>%
    arrange(plot, plot_section, month_order) %>%
    group_by(plot, plot_section, plot_section_label) %>%
    mutate(
      monitoring_step = row_number(),
      monitoring_label = if_else(
        monitoring_step == 1,
        str_c("Baseline\n", month),
        str_c("Follow-up ", monitoring_step - 1, "\n", month)
      )
    ) %>%
    ungroup()
}

outplant_plot_short_cumulative_survival <- function(cumulative_survival_summary) {
  cumulative_survival_summary %>%
    outplant_add_monitoring_labels() %>%
    mutate(monitoring_label = fct_reorder(monitoring_label, monitoring_step)) %>%
    ggplot(aes(x = monitoring_label, y = percent_cumulative_survival, group = plot_section_label)) +
    geom_errorbar(
      aes(ymin = percent_cumulative_survival_low, ymax = percent_cumulative_survival_high),
      width = 0.12,
      color = "grey25",
      linewidth = 0.5
    ) +
    geom_line(aes(color = plot_section_label), linewidth = 0.7) +
    geom_point(aes(color = plot_section_label), size = 2.8) +
    geom_text(aes(label = sprintf("%.1f%%", percent_cumulative_survival)), vjust = -1, size = 3.4) +
    facet_wrap(vars(plot), scales = "free_x") +
    scale_y_continuous(
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      x = "Monitoring step",
      y = "Cumulative survivorship",
      color = "Plot",
      title = "Short-monitoring cumulative survivorship",
      subtitle = "Plot C and Plot G are shown by baseline/follow-up step so their early results are not stretched across the full date axis"
    ) +
    scale_color_brewer(palette = "Dark2") +
    outplant_theme()
}

outplant_plot_interval_survival <- function(survival_summary) {
  survival_summary %>%
    filter(n_start > 0, !is.na(percent_survival)) %>%
    mutate(interval_label = fct_reorder(interval_label, interval_order)) %>%
    ggplot(aes(x = interval_label, y = percent_survival, group = plot_section_label)) +
    geom_errorbar(
      aes(ymin = percent_survival_low, ymax = percent_survival_high),
      width = 0.14,
      color = "grey25",
      linewidth = 0.5
    ) +
    geom_line(aes(color = plot_section_label), linewidth = 0.7) +
    geom_point(aes(color = plot_section_label), size = 2.8) +
    geom_text(aes(label = sprintf("%.1f%%", percent_survival)), vjust = -1, size = 3.4) +
    scale_y_continuous(
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      x = "Survey interval",
      y = "Interval survivorship",
      color = "Plot",
      title = "Outplant interval survivorship",
      subtitle = "Survival between adjacent TagLab match files; bars are exact binomial 95% CIs"
    ) +
    scale_color_brewer(palette = "Dark2") +
    outplant_theme() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
}

outplant_plot_short_interval_survival <- function(survival_summary) {
  survival_summary %>%
    filter(n_start > 0, !is.na(percent_survival)) %>%
    mutate(interval_label = fct_reorder(interval_label, interval_order)) %>%
    ggplot(aes(x = interval_label, y = percent_survival, fill = plot_section_label)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.64, color = "white") +
    geom_errorbar(
      aes(ymin = percent_survival_low, ymax = percent_survival_high),
      position = position_dodge(width = 0.72),
      width = 0.16,
      color = "grey25",
      linewidth = 0.5
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", percent_survival)),
      position = position_dodge(width = 0.72),
      vjust = -0.6,
      size = 3.4
    ) +
    facet_wrap(vars(plot), scales = "free_x") +
    scale_y_continuous(
      limits = c(0, 100),
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0.02, 0.1))
    ) +
    labs(
      x = "Survey interval",
      y = "Interval survivorship",
      fill = "Plot",
      title = "Short-monitoring interval survivorship",
      subtitle = "Intervals that start before outplanting are omitted from the figure because survival cannot be estimated when start n = 0"
    ) +
    scale_fill_brewer(palette = "Dark2") +
    outplant_theme() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))
}

outplant_plot_cover <- function(cover_summary) {
  cover_summary %>%
    mutate(month = fct_reorder(month, month_order)) %>%
    ggplot(aes(x = month, y = percent_outplant_cover, group = plot_section_label)) +
    geom_line(aes(color = plot_section_label), linewidth = 0.7) +
    geom_point(aes(color = plot_section_label), size = 2.8) +
    geom_text(aes(label = sprintf("%.4f%%", percent_outplant_cover)), vjust = -1, size = 3.2) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(
      x = "Survey month",
      y = "Outplant cover of plot",
      color = "Plot",
      title = "Outplant coral cover through time",
      subtitle = "Cover = summed outplant planar area / 480 m2 plot area"
    ) +
    scale_color_brewer(palette = "Dark2") +
    outplant_theme()
}

outplant_plot_species_prevalence <- function(species_prevalence) {
  species_prevalence %>%
    mutate(month = fct_reorder(month, month_order)) %>%
    ggplot(aes(x = month, y = percent_prevalence, fill = species)) +
    geom_col(color = "white", linewidth = 0.25, width = 0.72) +
    facet_wrap(vars(plot_section_label), ncol = 1) +
    scale_fill_manual(values = outplant_species_palette, drop = FALSE) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      x = "Survey month",
      y = "Species prevalence",
      fill = "Species",
      title = "Outplant species prevalence through time",
      subtitle = "Percent of present outplant annotations by species"
    ) +
    outplant_theme()
}

outplant_plot_species_cover <- function(species_prevalence) {
  species_prevalence %>%
    mutate(month = fct_reorder(month, month_order)) %>%
    ggplot(aes(x = month, y = percent_cover_within_outplants, fill = species)) +
    geom_col(color = "white", linewidth = 0.25, width = 0.72) +
    facet_wrap(vars(plot_section_label), ncol = 1) +
    scale_fill_manual(values = outplant_species_palette, drop = FALSE) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      x = "Survey month",
      y = "Share of outplant cover",
      fill = "Species",
      title = "Outplant cover composition through time",
      subtitle = "Percent of summed outplant planar area by species"
    ) +
    outplant_theme()
}

# Main pipeline -----------------------------------------------------------------
# Running this script rebuilds all outplant-only interval outputs.
outplant_setup_output_dirs()

outplant_config <- outplant_read_config(outplant_config_path)
outplant_depth_metadata <- outplant_read_depth_metadata(outplant_depth_path)
outplant_plot_depth_summary <- outplant_summarize_plot_depth(outplant_depth_metadata)
outplant_file_audit <- outplant_build_file_audit(outplant_config)
outplant_interval_rows <- outplant_build_interval_rows(outplant_config)
outplant_section_coral_intervals <- outplant_build_coral_intervals(outplant_interval_rows)
outplant_section_monthly_observations <- outplant_build_monthly_observations(outplant_section_coral_intervals)
outplant_coral_intervals <- outplant_combine_plot_sections(outplant_section_coral_intervals)
outplant_monthly_observations <- outplant_build_monthly_observations(outplant_coral_intervals)

outplant_section_cumulative_survival_summary <- outplant_summarize_cumulative_survival(
  outplant_section_monthly_observations,
  outplant_section_coral_intervals
)
outplant_cumulative_survival_summary <- outplant_summarize_cumulative_survival_by_plot(
  outplant_section_cumulative_survival_summary
)
outplant_temperature_observations <- outplant_read_temperature_file(outplant_temperature_path)
outplant_daily_temperature_summary <- outplant_summarize_daily_temperature(
  outplant_temperature_observations,
  outplant_cumulative_survival_summary
)
outplant_section_survival_summary <- outplant_summarize_survival(outplant_section_coral_intervals)
outplant_survival_summary <- outplant_summarize_survival_by_plot(outplant_section_survival_summary)
outplant_section_species_survival_summary <- outplant_summarize_species_survival(outplant_section_coral_intervals)
outplant_species_survival_summary <- outplant_summarize_species_survival_by_plot(outplant_section_species_survival_summary)
outplant_cover_summary <- outplant_summarize_cover(outplant_monthly_observations)
outplant_species_prevalence_summary <- outplant_summarize_species_prevalence(outplant_monthly_observations)
outplant_species_depth_summary <- outplant_summarize_species_depth(
  outplant_species_prevalence_summary,
  outplant_plot_depth_summary
)

outplant_master_tracking_dataset <- outplant_build_master_tracking_dataset(outplant_monthly_observations)
outplant_master_summary_dataset <- outplant_build_master_summary_dataset(
  outplant_cumulative_survival_summary,
  outplant_survival_summary,
  outplant_species_survival_summary,
  outplant_cover_summary,
  outplant_species_prevalence_summary
)

outplant_qa_tables <- outplant_build_qa(
  outplant_interval_rows,
  outplant_section_monthly_observations,
  outplant_section_coral_intervals,
  outplant_file_audit
)
list2env(outplant_qa_tables, envir = environment())

outplant_cumulative_survival_plot <- outplant_plot_cumulative_survival(
  outplant_cumulative_survival_summary,
  outplant_daily_temperature_summary
)
outplant_survival_plot <- outplant_plot_interval_survival(outplant_survival_summary)
outplant_cover_plot <- outplant_plot_cover(outplant_cover_summary)
outplant_species_prevalence_plot <- outplant_plot_species_prevalence(outplant_species_prevalence_summary)
outplant_species_cover_plot <- outplant_plot_species_cover(outplant_species_prevalence_summary)

outplant_write_named_csvs(outplant_qa_tables, outplant_table_dir)

outplant_write_named_csvs(
  list(
    outplant_interval_rows = outplant_interval_rows,
    outplant_section_coral_intervals = outplant_section_coral_intervals,
    outplant_section_monthly_observations = outplant_section_monthly_observations,
    outplant_coral_intervals = outplant_coral_intervals,
    outplant_monthly_observations = outplant_monthly_observations,
    outplant_master_tracking_dataset = outplant_master_tracking_dataset,
    outplant_depth_metadata = outplant_depth_metadata,
    outplant_temperature_observations = outplant_temperature_observations
  ),
  outplant_processed_dir
)

outplant_write_named_csvs(
  list(
    outplant_cumulative_survival_summary = outplant_cumulative_survival_summary,
    outplant_section_cumulative_survival_summary = outplant_section_cumulative_survival_summary,
    outplant_survival_summary = outplant_survival_summary,
    outplant_section_survival_summary = outplant_section_survival_summary,
    outplant_species_survival_summary = outplant_species_survival_summary,
    outplant_section_species_survival_summary = outplant_section_species_survival_summary,
    outplant_cover_summary = outplant_cover_summary,
    outplant_species_prevalence_summary = outplant_species_prevalence_summary,
    outplant_plot_depth_summary = outplant_plot_depth_summary,
    outplant_species_depth_summary = outplant_species_depth_summary,
    outplant_daily_temperature_summary = outplant_daily_temperature_summary,
    outplant_master_summary_dataset = outplant_master_summary_dataset
  ),
  outplant_table_dir
)

outplant_save_named_plots(
  list(
    outplant_cumulative_survival_plot = outplant_cumulative_survival_plot,
    outplant_survival_plot = outplant_survival_plot,
    outplant_cover_plot = outplant_cover_plot,
    outplant_species_prevalence_plot = outplant_species_prevalence_plot,
    outplant_species_cover_plot = outplant_species_cover_plot
  ),
  outplant_figure_dir,
  width = 8,
  height = 4.8
)

message("Built outplant-only interval workflow outputs.")
