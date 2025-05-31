# ----------------------------------------------------
# Script 1: Scoring and Reverse Coding of Instruments
# ----------------------------------------------------

# 1. Install and load libraries
required_packages <- c("readr", "tidyverse", "janitor")
installed <- installed.packages()[, "Package"]
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg, dependencies = TRUE)
}
library(readr)
library(tidyverse)
library(janitor)

# 2. Load and clean data
df <- read_csv2("HEDB.csv", locale = locale(encoding = "UTF-8")) %>%
  clean_names() %>%
  mutate(across(where(is.character), ~na_if(.x, ""))) %>%
  drop_na(id)

# 3. Reverse scoring from _reverse columns
reverse_items <- list(
  mbi4 = "mbi4_reverse", mbi7 = "mbi7_reverse", mbi9 = "mbi9_reverse", mbi12 = "mbi12_reverse",
  mbi17 = "mbi17_reverse", mbi18 = "mbi18_reverse", mbi19 = "mbi19_reverse", mbi21 = "mbi21_reverse",
  pwb2 = "pwb2_reverse", pwb4 = "pwb4_reverse", pwb7 = "pwb7_reverse", pwb9 = "pwb9_reverse",
  pwb11 = "pwb11_reverse", pwb13 = "pwb13_reverse", pwb15 = "pwb15_reverse", pwb17 = "pwb17_reverse",
  pwb18 = "pwb18_reverse", pwb20 = "pwb20_reverse", pwb22 = "pwb22_reverse", pwb24 = "pwb24_reverse",
  pwb27 = "pwb27_reverse", pwb29 = "pwb29_reverse", pwb31 = "pwb31_reverse", pwb32 = "pwb32_reverse",
  pwb34 = "pwb34_reverse", pwb35 = "pwb35_reverse", pwb41 = "pwb41_reverse", pwb42 = "pwb42_reverse",
  pwb43 = "pwb43_reverse", pwb44 = "pwb44_reverse", pwb45 = "pwb45_reverse", pwb54 = "pwb54_reverse",
  pwb55 = "pwb55_reverse", pwb56 = "pwb56_reverse", pwb58 = "pwb58_reverse", pwb60 = "pwb60_reverse",
  pwb61 = "pwb61_reverse", pwb62 = "pwb62_reverse", pwb63 = "pwb63_reverse", pwb65 = "pwb65_reverse",
  pwb66 = "pwb66_reverse", pwb73 = "pwb73_reverse", pwb74 = "pwb74_reverse", pwb75 = "pwb75_reverse",
  pwb76 = "pwb76_reverse", pwb82 = "pwb82_reverse", pwb83 = "pwb83_reverse", pwb84 = "pwb84_reverse"
)
for (name in names(reverse_items)) {
  if (reverse_items[[name]] %in% names(df)) {
    df[[name]] <- 7 - df[[reverse_items[[name]]]]
  }
}

# 4. Define subscales
pwb_raw <- list(
  pwb_positive_relations = c("pwb1", "pwb19", "pwb25", "pwb37", "pwb49", "pwb67", "pwb69", "pwb70", "pwb71", "pwb72", "pwb77", "pwb78", "pwb79", "pwb80"),
  pwb_autonomy = c("pwb8", "pwb12", "pwb30", "pwb33", "pwb44", "pwb48", "pwb52", "pwb63", "pwb65", "pwb66", "pwb70", "pwb73", "pwb78", "pwb84"),
  pwb_environmental_mastery = c("pwb3", "pwb5", "pwb21", "pwb33", "pwb39", "pwb46", "pwb59", "pwb64", "pwb67", "pwb69", "pwb70", "pwb71", "pwb72", "pwb73"),
  pwb_personal_growth = c("pwb10", "pwb16", "pwb28", "pwb40", "pwb48", "pwb52", "pwb57", "pwb64", "pwb66", "pwb67", "pwb69", "pwb73", "pwb74", "pwb75"),
  pwb_purpose_in_life = c("pwb5", "pwb23", "pwb47", "pwb53", "pwb59", "pwb71", "pwb77", "pwb79", "pwb81", "pwb83", "pwb84", "pwb29", "pwb35", "pwb45"),
  pwb_self_acceptance = c("pwb6", "pwb24", "pwb30", "pwb36", "pwb48", "pwb78", "pwb63", "pwb65", "pwb66", "pwb74", "pwb75", "pwb76", "pwb82", "pwb83")
)
res_raw <- list(
  res_meaningfulness = c("res16", "res21", "res22", "res25"),
  res_equanimity = c("res7", "res8", "res11", "res12"),
  res_existential_aloneness = c("res3", "res5", "res9"),
  res_self_reliance = c("res6", "res13", "res17", "res18", "res24"),
  res_perseverance = c("res1", "res2", "res4", "res14", "res15", "res20", "res23")
)
mbi_raw <- list(
  mbi_emotional_exhaustion = c("mbi1", "mbi2", "mbi3", "mbi8", "mbi13", "mbi14", "mbi16", "mbi20", "mbi22"),
  mbi_depersonalization = c("mbi5", "mbi10", "mbi11", "mbi15"),
  mbi_reduced_personal_accomplish = c("mbi4", "mbi7", "mbi9", "mbi12", "mbi17", "mbi18", "mbi19", "mbi21")
)

# 5. Function to compute raw scores
compute_raw_scores <- function(df, item_list, label) {
  for (name in names(item_list)) {
    valid_items <- item_list[[name]][item_list[[name]] %in% names(df)]
    df[[paste0("raw_", name)]] <- rowSums(df[, valid_items], na.rm = TRUE)
  }
  total_name <- paste0("raw_total_", label)
  df[[total_name]] <- rowSums(df[, paste0("raw_", names(item_list))], na.rm = TRUE)
  return(df)
}

df <- compute_raw_scores(df, pwb_raw, "pwb")
df <- compute_raw_scores(df, res_raw, "res")
df <- compute_raw_scores(df, mbi_raw, "mbi")

# 6. Save to new CSV file for further analysis
write_csv(df, "scored_data.csv")
cat("✅ Scored data saved as 'scored_data.csv'\n")
