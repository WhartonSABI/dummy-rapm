stages <- list(
  list(
    script = "tests/test_rapm_utils.R",
    required_outputs = character()
  ),
  list(
    script = "scripts/run_model_grid.R",
    required_outputs = c(
      "results/intermediate/broad_model_grid.csv",
      "results/intermediate/broad_configuration_summary.csv",
      "results/intermediate/broad_sample_summary.csv",
      "results/intermediate/broad_category_exposure.csv"
    )
  ),
  list(
    script = "scripts/tune_dummy_penalty.R",
    required_outputs = c(
      "results/intermediate/penalty_grid.csv",
      "results/intermediate/penalty_selected_summary.csv",
      paste0(
        "results/intermediate/",
        "penalty_selected_coefficients.csv"
      )
    )
  ),
  list(
    script = "scripts/build_results.R",
    required_outputs = c(
      "results/selected_configuration.csv",
      "results/season_results.csv",
      "manuscript/results.tex"
    )
  )
)

for (stage in stages) {
  script <- stage$script
  cat("\nRunning", script, "\n")
  stage_started <- Sys.time()
  status <- system2(
    command = "Rscript",
    args = c("--vanilla", script),
    stdout = "",
    stderr = ""
  )
  outputs_are_fresh <- (
    length(stage$required_outputs) > 0L &&
      all(file.exists(stage$required_outputs)) &&
      all(
        file.info(
          stage$required_outputs
        )$mtime >= stage_started
      )
  )
  if (status != 0L && !outputs_are_fresh) {
    stop(
      paste("Analysis failed in", script),
      call. = FALSE
    )
  }
  if (status != 0L && outputs_are_fresh) {
    warning(
      paste(
        script,
        "returned a nonzero status after writing all",
        "required fresh outputs; continuing."
      ),
      call. = FALSE
    )
  }
}

dir.create(
  "output/pdf",
  showWarnings = FALSE,
  recursive = TRUE
)
original_directory <- getwd()
setwd("manuscript")
latex_status <- system2(
  command = "latexmk",
  args = c("-pdf", "-interaction=nonstopmode", "main.tex"),
  stdout = "",
  stderr = ""
)
setwd(original_directory)
if (latex_status != 0L) {
  stop("Manuscript compilation failed.", call. = FALSE)
}
if (
  !file.copy(
    "manuscript/main.pdf",
    "output/pdf/dummy_rapm.pdf",
    overwrite = TRUE
  )
) {
  stop("Could not finalize output/pdf/dummy_rapm.pdf.", call. = FALSE)
}

cat(
  paste(
    "\nAnalysis complete. Final outputs are in",
    "results/, figures/, and output/pdf/.\n"
  )
)
