
install.packages("infotheo")
install.packages("dplyr")
install.packages("VIM")


library(dplyr)
library(infotheo)
library(VIM)


data <- read.csv("D:/DS lab/student_data.csv",header = TRUE,sep = ",")
print(data)


colSums(is.na(data))



num_data <- select_if(data, is.numeric)
target <- num_data$G3  


cat("Pearson Correlation with G3:\n")
pearson <- sapply(num_data, function(x) cor(x, target, method = "pearson"))
print(pearson)


cat("\nSpearman Correlation with G3:\n")
spearman <- sapply(num_data, function(x) cor(x, target, method = "spearman"))
print(spearman)




cat_data <- select_if(data, is.character)


cat("\nANOVA Results (categorical features vs G3):\n")
for (col in names(cat_data)) {
  model <- aov(data$G3 ~ as.factor(cat_data[[col]]))
  p <- summary(model)[[1]][["Pr(>F)"]][1]
  cat(col, "- p-value:", round(p, 4), "\n")
}


cat("\nMutual Information with G3:\n")

G3_disc <- discretize(data$G3)
mi <- sapply(cat_data, function(col) {
  col_disc <- discretize(as.factor(col))
  mutinformation(col_disc, G3_disc)
})
print(mi)
