# Install packages if not installed
install.packages("ggplot2")
install.packages("e1071")

# Load libraries
library(ggplot2)
library(e1071)

# Load your dataset
data <- read.csv("D:/DS lab/student_data(Lab Project).csv")

# Optional: check column names
names(data)

# Turn off scientific notation
options(scipen = 999)

# ------------------------------
# Histogram of G3 (Final Grade)
# ------------------------------
hist(data$G3,
     main = "Histogram of Final Grades (G3)",
     xlab = "Final Grade (G3)",
     col = "skyblue", border = "black")

# ------------------------------
# Histogram + Density Line (Line Histogram)
# ------------------------------
hist(data$G3, probability = TRUE,
     main = "G3 with Density Line",
     xlab = "G3",
     col = "lightgreen", border = "black")
lines(density(data$G3, na.rm = TRUE), col = "red", lwd = 2)

# ------------------------------
# Skewness of G3
# ------------------------------
skew_val <- skewness(data$G3, na.rm = TRUE)
print(paste("Skewness:", round(skew_val, 3)))

if (skew_val > 0) {
  print("The distribution is positively skewed (right-skewed).")
} else if (skew_val < 0) {
  print("The distribution is negatively skewed (left-skewed).")
} else {
  print("The distribution is approximately symmetric.")
}

# ------------------------------
# Barplot for a Categorical Attribute (e.g., sex)
# ------------------------------
barplot(table(data$sex),
        col = "skyblue",
        main = "Bar Plot of Gender",
        xlab = "Sex",
        ylab = "Count")

# ------------------------------
# Boxplot for Numeric Attribute (e.g., absences)
# ------------------------------
boxplot(data$absences,
        main = "Boxplot of Absences",
        ylab = "Number of Absences",
        col = "orange",
        horizontal = TRUE)

# ------------------------------
# Scatter Plot (e.g., G1 vs G3)
# ------------------------------
plot(data$G1, data$G3,
     main = "Scatter Plot: G1 vs G3",
     xlab = "First Period Grade (G1)",
     ylab = "Final Grade (G3)",
     col = "blue",
     pch = 16)

# ------------------------------
# Violin + Box Plot (G3 by Sex)
# ------------------------------
ggplot(data, aes(x = sex, y = G3)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Violin Plot of G3 by Gender",
       x = "Sex",
       y = "Final Grade (G3)")
