# Dummy RAPM

This repository estimates a Regularized Adjusted Plus-Minus model that
represents omitted low-minute players with ten lineup-side count indicators:
five for the home lineup and five for the away lineup.

## Analysis specification

- Seasons: 2007–2019 and 2022–2025. The disrupted 2019–20 season and
  COVID-shortened 2020–21 season are excluded.
- Inner training: October–December.
- Inner validation: January–February.
- Outer training: October–February.
- Outer test: March–April.
- May–September: excluded.
- Outcome: home scoring margin per 100 team possessions.
- Selection and headline evaluation: aggregated full-game margin RMSE.
- Secondary descriptive metric: out-of-sample R².
- Low-minute threshold: selected from the broad inner-validation grid.
- Dummy-to-player ridge penalty ratio: selected from a 0.1-spaced grid from
  1.0 through 2.5, extended automatically if the winner is on a boundary.

The selected threshold and penalty are written to
`results/selected_configuration.csv`; neither is hardcoded in the results
builder or manuscript.

## Run the analysis

From the repository root:

```bash
Rscript --vanilla scripts/run_all.R
```

The model grid and penalty tuning use `parallel::detectCores() - 4` workers by
default (with a minimum of one). Set `RAPM_WORKERS` to override that value.
The complete runner executes tests, refits the analysis, regenerates tables and
figures, compiles the manuscript, and copies the checked artifact to
`output/pdf/dummy_rapm.pdf`.

Completed seasons are checkpointed, so an interrupted run resumes rather than
refitting finished seasons. Checkpoints include a specification version,
chronological-window definition, and common-fold scheme; incompatible fitted
checkpoints are ignored. Downloaded play-by-play and box-score caches are
retained.

To rebuild only the final tables and figures from existing model outputs:

```bash
Rscript --vanilla scripts/build_results.R
```

To compile the manuscript:

```bash
cd manuscript
latexmk -pdf main.tex
```

## Repository structure

- `R/rapm_utils.R`: lineup, stint, design-matrix, ridge, and validation helpers.
- `scripts/run_model_grid.R`: season data construction and threshold search.
- `scripts/tune_dummy_penalty.R`: focused 1.0–2.5 penalty search.
- `scripts/build_results.R`: final tables, coefficient summaries, and figures.
- `scripts/run_all.R`: complete reproducible pipeline.
- `tests/test_rapm_utils.R`: targeted regression tests.
- `results/`: final machine-readable result tables.
- `figures/`: final manuscript figures.
- `manuscript/`: LaTeX source and generated result macros.
- `output/pdf/`: compiled manuscript.
- `archive/`: legacy exploratory scripts and outputs retained for reference.
