library(tidyverse)

setwd("/Users/kennywatts/Documents/GitHub/Dummy-rapm")

# -----------------------------
# Load data
# -----------------------------
results <- read_csv("May_27_Model_Results.csv")

# -----------------------------
# Summary Table
# -----------------------------
summary_table <- results %>%
  group_by(model_type, min_minutes) %>%
  summarise(
    mean_RMSE = sprintf("%.2f", mean(RMSE, na.rm = TRUE)),
    sd_RMSE   = round(sd(RMSE, na.rm = TRUE), 2),
    mean_R2   = round(mean(R2, na.rm = TRUE), 3),
    sd_R2     = round(sd(R2, na.rm = TRUE), 3),
    n = n(),
    .groups = "drop"
  )

# -----------------------------
# Summarize across seasons
# -----------------------------
summary_results <- results %>%
  group_by(model_type, min_minutes) %>%
  summarise(
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    sd_RMSE   = sd(RMSE, na.rm = TRUE),
    se_RMSE   = sd_RMSE / sqrt(n()),
    ci_RMSE   = qt(0.975, df = n() - 1) * se_RMSE,
    mean_R2   = mean(R2, na.rm = TRUE),
    sd_R2     = sd(R2, na.rm = TRUE),
    se_R2     = sd_R2 / sqrt(n()),
    ci_R2     = qt(0.975, df = n() - 1) * se_R2,
    n = n(),
    .groups = "drop"
  )

# -----------------------------
# Compute paired differences
# -----------------------------
diff_results <- results %>%
  select(season, model_type, min_minutes, RMSE, R2) %>%
  pivot_wider(names_from = model_type, values_from = c(RMSE, R2)) %>%
  mutate(
    diff_RMSE = RMSE_no_dummy - RMSE_dummy,
    diff_R2   = R2_no_dummy   - R2_dummy
  )

diff_summary <- diff_results %>%
  group_by(min_minutes) %>%
  summarise(
    mean_diff_RMSE = mean(diff_RMSE, na.rm = TRUE),
    se_diff_RMSE   = sd(diff_RMSE, na.rm = TRUE) / sqrt(n()),
    ci_diff_RMSE   = qt(0.975, df = n() - 1) * se_diff_RMSE,
    mean_diff_R2   = mean(diff_R2, na.rm = TRUE),
    se_diff_R2     = sd(diff_R2, na.rm = TRUE) / sqrt(n()),
    ci_diff_R2     = qt(0.975, df = n() - 1) * se_diff_R2,
    .groups = "drop"
  )

# -----------------------------
# Paired t-tests + significance labels
# -----------------------------
sig_results <- diff_results %>%
  group_by(min_minutes) %>%
  summarise(
    p_RMSE = t.test(diff_RMSE)$p.value,
    p_R2   = t.test(diff_R2)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label_RMSE = case_when(
      p_RMSE < 0.001 ~ "***",
      p_RMSE < 0.01  ~ "**",
      p_RMSE < 0.05  ~ "*",
      TRUE           ~ "ns"
    ),
    label_R2 = case_when(
      p_R2 < 0.001 ~ "***",
      p_R2 < 0.01  ~ "**",
      p_R2 < 0.05  ~ "*",
      TRUE         ~ "ns"
    )
  )

# -----------------------------
# Ribbon data (gap between groups)
# -----------------------------
ribbon_data <- summary_results %>%
  select(model_type, min_minutes, mean_RMSE, mean_R2) %>%
  pivot_wider(names_from = model_type, values_from = c(mean_RMSE, mean_R2))

# -----------------------------
# Significance label positions
# -----------------------------
upper_env_RMSE <- summary_results %>%
  group_by(min_minutes) %>%
  summarise(label_y = max(mean_RMSE + ci_RMSE) * 1.02, .groups = "drop") %>%
  left_join(sig_results %>% select(min_minutes, label_RMSE), by = "min_minutes")

upper_env_R2 <- summary_results %>%
  group_by(min_minutes) %>%
  summarise(label_y = max(mean_R2 + ci_R2) * 1.02, .groups = "drop") %>%
  left_join(sig_results %>% select(min_minutes, label_R2), by = "min_minutes")

# -----------------------------
# Plot 1: RMSE with ribbon + significance
# -----------------------------
rmse_plot <- ggplot(summary_results, aes(x = min_minutes, y = mean_RMSE, color = model_type)) +
  geom_ribbon(
    data = ribbon_data,
    aes(x = min_minutes, ymin = mean_RMSE_dummy, ymax = mean_RMSE_no_dummy),
    inherit.aes = FALSE,
    fill = "gray80", alpha = 0.4
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_RMSE - ci_RMSE, ymax = mean_RMSE + ci_RMSE), width = 1) +
  geom_text(
    data = upper_env_RMSE,
    aes(x = min_minutes, y = label_y, label = label_RMSE),
    inherit.aes = FALSE, size = 5, color = "black"
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(
    title = "Average RMSE Across Seasons",
    subtitle = "Shaded band = gap between groups | * paired t-test: *p<.05  **p<.01  ***p<.001",
    x = "Minimum Minutes Threshold",
    y = "Average RMSE",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 14)

# -----------------------------
# Plot 2: R² with ribbon + significance
# -----------------------------
r2_plot <- ggplot(summary_results, aes(x = min_minutes, y = mean_R2, color = model_type)) +
  geom_ribbon(
    data = ribbon_data,
    aes(x = min_minutes, ymin = mean_R2_dummy, ymax = mean_R2_no_dummy),
    inherit.aes = FALSE,
    fill = "gray80", alpha = 0.4
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_R2 - ci_R2, ymax = mean_R2 + ci_R2), width = 1) +
  geom_text(
    data = upper_env_R2,
    aes(x = min_minutes, y = label_y, label = label_R2),
    inherit.aes = FALSE, size = 5, color = "black"
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(
    title = "Average R² Across Seasons",
    subtitle = "Shaded band = gap between groups | * paired t-test: *p<.05  **p<.01  ***p<.001",
    x = "Minimum Minutes Threshold",
    y = "Average R²",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 14)

# -----------------------------
# Plot 3: RMSE difference
# -----------------------------
rmse_diff_plot <- ggplot(diff_summary, aes(x = min_minutes, y = mean_diff_RMSE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = mean_diff_RMSE - ci_diff_RMSE,
                  ymax = mean_diff_RMSE + ci_diff_RMSE),
              alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(
    title = "RMSE Difference: no_dummy − dummy",
    subtitle = "CI excluding 0 indicates a consistent difference across seasons",
    x = "Minimum Minutes Threshold",
    y = "Mean Difference in RMSE"
  ) +
  theme_minimal(base_size = 14)

# -----------------------------
# Plot 4: R² difference
# -----------------------------
r2_diff_plot <- ggplot(diff_summary, aes(x = min_minutes, y = mean_diff_R2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = mean_diff_R2 - ci_diff_R2,
                  ymax = mean_diff_R2 + ci_diff_R2),
              alpha = 0.2, fill = "coral") +
  geom_line(color = "coral", linewidth = 1) +
  geom_point(color = "coral", size = 3) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(
    title = "R² Difference: no_dummy − dummy",
    subtitle = "CI excluding 0 indicates a consistent difference across seasons",
    x = "Minimum Minutes Threshold",
    y = "Mean Difference in R²"
  ) +
  theme_minimal(base_size = 14)

# Plot 5: Season by season differences

difference_by_season <- results %>%
  filter(min_minutes == 10) %>%
  ggplot(aes(x = season, y = RMSE, color = model_name, group = model_name)) +
  geom_line() +
  geom_point() +
  labs(
    title = "RMSE by Season (min_minutes = 10)",
    x = "Season",
    y = "RMSE",
    color = "Model"
  ) +
  theme_minimal()

# Plot 6: Does filtering (at any threshold) beat not filtering at all?
baseline_compare <- diff_results %>%
  group_by(season) %>%
  mutate(
    RMSE_baseline    = RMSE_dummy[min_minutes == 0],
    R2_baseline      = R2_dummy[min_minutes == 0],
    RMSE_improvement = RMSE_baseline - RMSE_dummy,   # positive = better than no filtering
    R2_improvement   = R2_dummy - R2_baseline        # positive = better than no filtering
  ) %>%
  ungroup()

baseline_long <- baseline_compare %>%
  select(season, min_minutes, RMSE_improvement, R2_improvement) %>%
  pivot_longer(cols = c(RMSE_improvement, R2_improvement), names_to = "metric", values_to = "value")

baseline_10_r2 <- baseline_long %>%
  filter(min_minutes == 10, metric == "R2_improvement")

threshold_vs_no_filter <- ggplot(baseline_10_r2, aes(x = factor(season), y = value, fill = value > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#d95f0e"), guide = "none") +
  labs(title = "Does a 10-Minute Threshold Beat Not Filtering At All?",
       subtitle = "Positive values = filtered dummy model (min_minutes = 10) outperforms min_minutes = 0",
       x = "Season", y = "R² improvement") +
  theme_minimal()

baseline_10_rmse <- baseline_long %>%
  filter(min_minutes == 10, metric == "RMSE_improvement")

threshold_vs_no_filter_rmse <- ggplot(baseline_10_rmse, aes(x = factor(season), y = value, fill = value > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#d95f0e"), guide = "none") +
  labs(title = "Does a 10-Minute Threshold Beat Not Filtering At All?",
       subtitle = "Positive values = filtered dummy model (min_minutes = 10) outperforms min_minutes = 0",
       x = "Season", y = "RMSE improvement") +
  theme_minimal()


# Proportion of times 10_dummy rmse < 10_no_dummy rmse

prop_lower <- results %>%
  filter(min_minutes == 10) %>%
  select(season, model_name, RMSE) %>%
  pivot_wider(names_from = model_name, values_from = RMSE) %>%
  summarise(prop = mean(`10_dummy` < `10_no_dummy`)) %>%
  pull(prop)

prop_lower


# -----------------------------
# Display
# -----------------------------
rmse_plot
r2_plot
rmse_diff_plot
r2_diff_plot
difference_by_season
threshold_vs_no_filter
threshold_vs_no_filter_rmse

# -----------------------------
# Save plots to current directory
# -----------------------------

ggsave(
  filename = "rmse_plot.png",
  plot = rmse_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "r2_plot.png",
  plot = r2_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "rmse_diff_plot.png",
  plot = rmse_diff_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "r2_diff_plot.png",
  plot = r2_diff_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "difference_by_season_plot.png",
  plot = difference_by_season,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "threshold_vs_no_filter_plot.png",
  plot = threshold_vs_no_filter,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "threshold_vs_no_filter_rmse.png",
  plot = threshold_vs_no_filter_rmse,
  width = 8,
  height = 6,
  dpi = 300
)

