# ------------------------------------------------------------
# Correlation and Reliability Analysis: Subscales + Totals
# ------------------------------------------------------------

# 1. Install required packages if not present
required_packages <- c("tidyverse", "ggcorrplot", "psych")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# 2. Load libraries
library(tidyverse)
library(ggcorrplot)
library(psych)

# 3. Select subscales and total scores (raw data)
cor_data <- score_vars %>%
  select(
    starts_with("raw_pwb_"), raw_total_pwb,
    starts_with("raw_res_"), raw_total_res,
    starts_with("raw_mbi_"), raw_total_mbi
  ) %>%
  select(where(is.numeric))

# 4. Compute Pearson correlation matrix (pairwise NAs)
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = "pearson")

# 5. Print rounded correlation matrix
cat("📈 Correlation matrix between subscales and total scores:\n")
print(round(cor_matrix, 2))

# 6. Visualize correlation heatmap
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

# ------------------------------------------------------------
# 7. Reliability: Cronbach's Alpha by Subscale and Total Score
# ------------------------------------------------------------

cat("\n🧪 Cronbach's Alpha for Subscales and Total Scores:\n")

# Subscale definitions in lowercase
pwb_subscales <- list(
  autonomy = c("pwb1", "pwb19", "pwb25", "pwb37", "pwb49", "pwb67", "pwb69", "pwb70", "pwb71", "pwb72", "pwb77", "pwb78", "pwb79", "pwb80"),
  environmental_mastery = c("pwb6", "pwb14", "pwb22", "pwb24", "pwb28", "pwb30", "pwb36", "pwb50", "pwb66", "pwb68", "pwb70", "pwb73", "pwb74", "pwb75"),
  personal_growth = c("pwb3", "pwb5", "pwb21", "pwb33", "pwb39", "pwb46", "pwb59", "pwb64", "pwb67", "pwb69", "pwb70", "pwb71", "pwb72", "pwb73"),
  positive_relations = c("pwb10", "pwb16", "pwb28", "pwb40", "pwb48", "pwb52", "pwb57", "pwb64", "pwb66", "pwb67", "pwb69", "pwb73", "pwb74", "pwb75"),
  purpose_in_life = c("pwb11", "pwb17", "pwb29", "pwb35", "pwb41", "pwb47", "pwb51", "pwb53", "pwb61", "pwb64", "pwb66", "pwb69", "pwb70", "pwb71"),
  self_acceptance = c("pwb8", "pwb12", "pwb30", "pwb33", "pwb44", "pwb48", "pwb52", "pwb63", "pwb65", "pwb66", "pwb70", "pwb73", "pwb78", "pwb84")
)

res_subscales <- list(
  meaningful_life = c("res16", "res21", "res22", "res25"),
  perseverance = c("res7", "res8", "res11", "res12"),
  self_esteem = c("res3", "res5", "res9"),
  emotional_control = c("res6", "res13", "res17", "res18", "res24"),
  personal_competence = c("res1", "res2", "res4", "res14", "res15", "res20", "res23")
)

mbi_subscales <- list(
  emotional_exhaustion = c("mbi1", "mbi2", "mbi3", "mbi6", "mbi13", "mbi14", "mbi16", "mbi20", "mbi22"),
  depersonalization = c("mbi5", "mbi10", "mbi11", "mbi15"),
  reduced_accomplishment = c("mbi8", "mbi17", "mbi18", "mbi19", "mbi21")
)

# Function to compute and print alpha
print_alphas <- function(data, sublist, label) {
  cat(paste0("\n--- ", label, " ---\n"))
  for (sub in names(sublist)) {
    items <- sublist[[sub]]
    valid_items <- items[items %in% names(data)]
    if (length(valid_items) > 1) {
      alpha_result <- psych::alpha(data[, valid_items], warnings = FALSE)
      cat(paste0("✔ ", sub, ": α = ", round(alpha_result$total$raw_alpha, 3), "\n"))
    } else {
      cat(paste0("⚠ ", sub, ": Not enough valid items found.\n"))
    }
  }
}

# Run alpha analysis
print_alphas(df, pwb_subscales, "PWB Subscales")
print_alphas(df, res_subscales, "RES Subscales")
print_alphas(df, mbi_subscales, "MBI Subscales")

# Alpha for full instruments
cat("\n--- Total Instrument Alphas ---\n")
cat(paste0("✔ PWB Total: α = ", round(psych::alpha(df %>% select(all_of(unlist(pwb_subscales))))$total$raw_alpha, 3), "\n"))
cat(paste0("✔ RES Total: α = ", round(psych::alpha(df %>% select(all_of(unlist(res_subscales))))$total$raw_alpha, 3), "\n"))
cat(paste0("✔ MBI Total: α = ", round(psych::alpha(df %>% select(all_of(unlist(mbi_subscales))))$total$raw_alpha, 3), "\n"))
