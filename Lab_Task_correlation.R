library(dplyr)
library(ggplot2)
library(tidyr)


df <- read.csv("C:/Users/HP/Downloads/student/Student Depression Dataset.csv", header = TRUE, sep = ",")
df <- na.omit(df)

dependent_var <- "Depression"
df[[dependent_var]] <- as.factor(df[[dependent_var]])

categorical_vars <- df %>% select_if(is.character) %>% colnames()
categorical_vars <- c(categorical_vars, "Depression")
numerical_vars <- df %>% select_if(is.numeric) %>% colnames()
numerical_vars <- setdiff(numerical_vars, dependent_var)


spearman_corr <- cor(df[numerical_vars], method = "spearman", use = "complete.obs")

var <- numerical_vars[3]
correlation_data <- data.frame(
  Variable = rownames(spearman_corr),
  Correlation = spearman_corr[, var]
)

ggplot(correlation_data, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", aes(fill = Correlation > 0)) +
  labs(
    title = paste("Correlation with", var, "(Spearman)"),
    x = "Variables",
    y = paste("Correlation with", var)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#E79796", "#F5CEC7")) +
  coord_cartesian(ylim = c(-0.5, 1))


anova_results <- list()
for (var in numerical_vars) {
  formula <- as.formula(paste(var, "~", dependent_var))
  anova_result <- aov(formula, data = df)
  summary_result <- summary(anova_result)
  anova_results[[var]] <- data.frame(
    Variable = var,
    F_Score = summary_result[[1]]$`F value`[1],
    P_Value = summary_result[[1]]$`Pr(>F)`[1]
  )
}

anova_df <- bind_rows(anova_results)
anova_df <- anova_df[order(anova_df$P_Value), ]

ggplot(anova_df, aes(x = reorder(Variable, -F_Score), y = F_Score, fill = P_Value < 0.05)) +
  geom_bar(stat = "identity") +
  labs(
    title = paste("ANOVA Results (", dependent_var, ")", sep = ""),
    x = "Variables",
    y = "F-Score",
    fill = "Significant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#455054", "#308695"))



chi_square_results <- list()
for (var_name in categorical_vars) {
  if (var_name != dependent_var) {
    contingency_table <- table(df[[var_name]], df[[dependent_var]])
    test_result <- chisq.test(contingency_table)
    chi_square_results[[var_name]] <- data.frame(
      Variable = var_name,
      Chi_Square_Val = test_result$statistic,
      P_Value = test_result$p.value
    )
  }
}

chi_square_df <- bind_rows(chi_square_results)
chi_square_df <- chi_square_df[order(chi_square_df$P_Value), ]

ggplot(chi_square_df, aes(x = reorder(Variable, -Chi_Square_Val), y = Chi_Square_Val, fill = P_Value < 0.05)) +
  geom_bar(stat = "identity") +
  labs(
    title = paste("Chi-Square Results (", dependent_var, ")", sep = ""),
    x = "Variables",
    y = "Chi-Square value",
    fill = "Significant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#D87F81", "#EAB595"))