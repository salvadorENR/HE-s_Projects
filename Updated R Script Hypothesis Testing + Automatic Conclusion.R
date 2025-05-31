# ------------------------------------------------------------
# Hypothesis Testing: Does RES vary by Age or Teaching Experience?
# ------------------------------------------------------------

# 1. Create grouping variables
df <- df %>%
  mutate(
    age_group = case_when(
      age < 30 ~ "<30",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 ~ "50+"
    ),
    experience_group = case_when(
      teaching_experience < 5 ~ "<5 years",
      teaching_experience >= 5 & teaching_experience < 15 ~ "5-14 years",
      teaching_experience >= 15 & teaching_experience < 25 ~ "15-24 years",
      teaching_experience >= 25 ~ "25+ years"
    )
  )

# 2. Check group sizes (optional)
cat("\n👥 Group sizes:\n")
print(table(df$age_group))
print(table(df$experience_group))

# 3. Run ANOVA: RES vs Age Group
anova_age <- aov(raw_total_res ~ age_group, data = df)
summary_age <- summary(anova_age)
p_age <- summary_age[[1]]$`Pr(>F)`[1]

# 4. Run ANOVA: RES vs Teaching Experience Group
anova_exp <- aov(raw_total_res ~ experience_group, data = df)
summary_exp <- summary(anova_exp)
p_exp <- summary_exp[[1]]$`Pr(>F)`[1]

# 5. Print results with conclusion
cat("\n📊 ANOVA Results for Age Group:\n")
print(summary_age)
if (p_age < 0.05) {
  cat("✅ Conclusion: There is a significant difference in resilience across age groups (p =", round(p_age, 4), ").\n")
  cat("📌 You may now explore post-hoc tests (TukeyHSD).\n")
} else {
  cat("❌ Conclusion: No significant difference in resilience across age groups (p =", round(p_age, 4), ").\n")
}

cat("\n📊 ANOVA Results for Teaching Experience:\n")
print(summary_exp)
if (p_exp < 0.05) {
  cat("✅ Conclusion: There is a significant difference in resilience across experience groups (p =", round(p_exp, 4), ").\n")
  cat("📌 You may now explore post-hoc tests (TukeyHSD).\n")
} else {
  cat("❌ Conclusion: No significant difference in resilience across experience groups (p =", round(p_exp, 4), ").\n")
}

# 6. Optional post-hoc test if significant
if (p_age < 0.05) {
  cat("\n🔍 Post-hoc Tukey HSD for Age Groups:\n")
  print(TukeyHSD(anova_age))
}
if (p_exp < 0.05) {
  cat("\n🔍 Post-hoc Tukey HSD for Experience Groups:\n")
  print(TukeyHSD(anova_exp))
}

# 7. Boxplots
library(ggplot2)

ggplot(df, aes(x = age_group, y = raw_total_res)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Resilience by Age Group", x = "Age Group", y = "Total Resilience Score") +
  theme_minimal()

ggplot(df, aes(x = experience_group, y = raw_total_res)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Resilience by Teaching Experience", x = "Experience Group", y = "Total Resilience Score") +
  theme_minimal()
# ------------------------------------------------------------
# 8. Non-parametric Alternative: Kruskal-Wallis Tests
# ------------------------------------------------------------

cat("\n📊 Non-Parametric Analysis (Kruskal-Wallis):\n")

# Kruskal-Wallis test for Age Group
kw_age <- kruskal.test(raw_total_res ~ age_group, data = df)
cat("\n📌 Kruskal-Wallis Test: Resilience by Age Group\n")
print(kw_age)
if (kw_age$p.value < 0.05) {
  cat("✅ Conclusion: Significant difference in resilience across age groups (p =", round(kw_age$p.value, 4), ").\n")
} else {
  cat("❌ Conclusion: No significant difference in resilience across age groups (p =", round(kw_age$p.value, 4), ").\n")
}

# Kruskal-Wallis test for Experience Group
kw_exp <- kruskal.test(raw_total_res ~ experience_group, data = df)
cat("\n📌 Kruskal-Wallis Test: Resilience by Teaching Experience\n")
print(kw_exp)
if (kw_exp$p.value < 0.05) {
  cat("✅ Conclusion: Significant difference in resilience across experience groups (p =", round(kw_exp$p.value, 4), ").\n")
} else {
  cat("❌ Conclusion: No significant difference in resilience across experience groups (p =", round(kw_exp$p.value, 4), ").\n")
}
