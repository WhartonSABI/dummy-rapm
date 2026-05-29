# Coefficient Analysis

# Combine all CSVs
results <- bind_rows(
  read_csv("final_coefs_2005_2009.csv"),
  read_csv("final_coefs_2010_2014.csv"),
  read_csv("final_coefs_2015_2019.csv"),
  read_csv("final_coefs_2021_2025.csv")
)

# Filter for dummy players (1-5) at min_minutes = 10
dummy_player_coefs <- results %>%
  filter(
    model == "dummy",
    min_minutes == 10,
    player_id %in% c("1", "2", "3", "4", "5")
  )

# Summary table with mean and 95% confidence intervals
dummy_summary <- dummy_player_coefs %>%
  group_by(model, player_id) %>%
  summarise(
    n         = n(),
    mean_coef = mean(coefficient, na.rm = TRUE),
    se        = sd(coefficient, na.rm = TRUE) / sqrt(n),
    ci_lower  = mean_coef - qt(0.975, df = n - 1) * se,
    ci_upper  = mean_coef + qt(0.975, df = n - 1) * se,
    p_value   = t.test(coefficient, mu = 0, alternative = "less")$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    player_label = paste0("Dummy (", player_id, " unqualified)"),
    across(c(mean_coef, ci_lower, ci_upper), ~ round(., 3)),
    p_value = round(p_value, 8)
  ) %>%
  arrange(model, player_id)

print(dummy_summary)
