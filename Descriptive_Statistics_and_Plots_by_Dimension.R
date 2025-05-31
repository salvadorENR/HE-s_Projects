# ----------------------------------------------------------
# Script Title: Descriptive Statistics, Boxplots & Densities
# For: PWB, RES, and MBI Scales (Subscales and Total Scores)
# ----------------------------------------------------------

# 1. Load required libraries (assumes packages already installed)
library(tidyverse)
library(janitor)
library(ggplot2)

# 2. Load the scored dataset
df <- read_csv("scored_data.csv")

# 3. Extract only relevant score variables
score_vars <- df %>%
  select(id, starts_with("raw_pwb_"), raw_total_pwb,
         starts_with("raw_res_"), raw_total_res,
         starts_with("raw_mbi_"), raw_total_mbi)

# 4. Create long-format data and assign instrument group
score_long <- score_vars %>%
  pivot_longer(-id, names_to = "dimension", values_to = "score") %>%
  mutate(
    instrument = case_when(
      str_detect(dimension, "pwb") ~ "PWB",
      str_detect(dimension, "res") ~ "RES",
      str_detect(dimension, "mbi") ~ "MBI",
      TRUE ~ "Other"
    )
  )

# 5. Generate summary statistics by dimension
summary_stats <- score_long %>%
  group_by(instrument, dimension) %>%
  summarise(
    n = sum(!is.na(score)),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    min = min(score, na.rm = TRUE),
    q25 = quantile(score, 0.25, na.rm = TRUE),
    median = median(score, na.rm = TRUE),
    q75 = quantile(score, 0.75, na.rm = TRUE),
    max = max(score, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Show summary statistics
cat("📊 Summary Statistics by Instrument and Dimension:\n")
print(summary_stats)

# 7. Separate boxplots per instrument
instrument_list <- unique(score_long$instrument)

for (instr in instrument_list) {
  plot_data <- score_long %>% filter(instrument == instr)
  
  print(
    ggplot(plot_data, aes(x = dimension, y = score)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(
        title = paste("Boxplot of", instr, "Dimensions"),
        x = "Dimension",
        y = "Score"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# 8. Histograms for total scores
total_vars <- c("raw_total_pwb", "raw_total_res", "raw_total_mbi")

for (var in total_vars) {
  print(
    ggplot(score_vars, aes_string(x = var)) +
      geom_histogram(bins = 20, fill = "steelblue", color = "white") +
      labs(
        title = paste("Histogram of", toupper(var)),
        x = "Total Score",
        y = "Frequency"
      ) +
      theme_minimal()
  )
}

# 9. Density plots for each MBI subscale overlaid with PWB and RES total scores
mbi_subscales <- names(score_vars)[str_detect(names(score_vars), "^raw_mbi_") & names(score_vars) != "raw_total_mbi"]

for (mbi_var in mbi_subscales) {
  print(
    ggplot() +
      geom_density(data = score_vars, aes_string(x = mbi_var), color = "red", linewidth = 1.2) +
      geom_density(data = score_vars, aes(x = raw_total_pwb), color = "blue", linetype = "dashed") +
      geom_density(data = score_vars, aes(x = raw_total_res), color = "darkgreen", linetype = "dashed") +
      labs(
        title = paste("Density of", mbi_var, "vs. Total Scores of PWB and RES"),
        x = "Score",
        y = "Density"
      ) +
      theme_minimal()
  )
}

# 10. Standardized density plot with all 5 scores in one chart
standardized_scores <- score_vars %>%
  mutate(across(
    c(
      raw_mbi_depersonalization,
      raw_mbi_emotional_exhaustion,
      raw_mbi_reduced_personal_accomplish,
      raw_total_pwb,
      raw_total_res
    ),
    ~ scale(.)[, 1]
  )) %>%
  select(
    Depersonalization = raw_mbi_depersonalization,
    Emotional_Exhaustion = raw_mbi_emotional_exhaustion,
    Reduced_Personal_Accomplishment = raw_mbi_reduced_personal_accomplish,
    Total_PWB = raw_total_pwb,
    Total_RES = raw_total_res
  ) %>%
  pivot_longer(cols = everything(), names_to = "Measure", values_to = "Score")

ggplot(standardized_scores, aes(x = Score, color = Measure)) +
  geom_density(linewidth = 1) +
  labs(
    title = "Standardized Density Plots: MBI Subscales vs. Total PWB and RES",
    x = "Standardized Score (Z-score)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )
