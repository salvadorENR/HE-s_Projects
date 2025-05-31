# ------------------------------------------------------------
# Correlation Analysis: All Subscales + Total Scores (Raw Data)
# ------------------------------------------------------------

# 1. Install required packages if not present
required_packages <- c("tidyverse", "ggcorrplot")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# 2. Load libraries
library(tidyverse)
library(ggcorrplot)

# 3. Select subscale and total scores (raw data)
cor_data <- score_vars %>%
  select(
    starts_with("raw_pwb_"), raw_total_pwb,
    starts_with("raw_res_"), raw_total_res,
    starts_with("raw_mbi_"), raw_total_mbi
  ) %>%
  select(where(is.numeric))

# 4. Compute Pearson correlation matrix (pairwise NAs)
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = "pearson")

# 5. Print rounded correlation matrix in console
cat("📈 Correlation matrix between subscales and total scores:\n")
print(round(cor_matrix, 2))

# 6. Visualize with correlation heatmap
ggcorrplot(
  cor_matrix,
  method = "circle",
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlation Matrix: Subscales and Total Scores",
  outline.color = "gray"
)
