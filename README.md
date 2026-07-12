# DPNR Outplant Analyses

This project analyzes coral outplant survivorship, cover, and species composition from TagLab match CSV files.

## How to Run

1. Put the full `Coral_survivorship_project` folder anywhere on your computer.
2. Open `CWORI_RStudio.Rproj` in RStudio.
3. Open `coral_survivorship_report.qmd`.
4. Click Render.

The report and R script automatically find the project folder by looking for `config/outplant_interval_files/`, so users should not need to edit hard-coded working-directory paths.

If rendering from the Terminal is easier, use the Quarto copy bundled with RStudio:

```bash
/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto render
```

The easiest local render command is:

```bash
bash scripts/render_site.sh
```

That command renders the real project root and checks that the custom homepage
was written to `docs/index.html`.

## Main Files

- `coral_survivorship_report.qmd`: Quarto report with tabs, figures, tables, and code dropdowns.
- `R/03_outplant_interval_workflow.R`: main analysis workflow.
- `config/outplant_interval_files/`: one user-editable TagLab match-file list per plot (`Plot_A.csv` through `Plot_H.csv`).
- `data_raw/taglab/`: raw TagLab match files.
- `data_processed/outplant_master_tracking_dataset.csv`: recommended detailed output dataset.
- `outputs/Tables/outplant_master_summary_dataset.csv`: recommended summary output dataset.

## Adding New Plot or Month Files

1. Anyone working on TagLab uploads new exports to the appropriate Teams folder under `Documents > General > Photogrammetry & Monitoring > Survivorship Exports`.
2. Add the file information to the appropriate plot config inside the Teams project copy.
3. Let Spencer know the files and config rows are ready.
4. Spencer runs `git pull` in the local Git project.
5. Spencer copies the matches CSVs and new config rows into the local project.
6. Render `coral_survivorship_report.qmd`.
7. Check the file audit and QA/QC tables before committing and pushing the update.
8. Run `Rscript scripts/pre_push_check.R` before pushing to confirm the rendered website and main outputs are present.

See the detailed **SOP** tab in the website for the full Teams-to-GitHub steps.

## Before Pushing to GitHub

Run this quick check from the project folder:

```bash
Rscript scripts/pre_push_check.R
```

The check confirms the expected GitHub remote, important rendered pages, main output tables, and QA file-audit results. It also prints the current Git status so it is clear what still needs to be staged or committed.
