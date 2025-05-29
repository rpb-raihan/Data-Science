install.packages("metan")
install.packages("AgroR")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("patchwork")
install.packages("tidyr")

library(metan)
library(AgroR)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

mydata <- read.csv("C:/Users/HP/Downloads/student/Student Depression Dataset.csv", header = TRUE, sep = ",")
mydata
View(mydata)
str(mydata)

total_missing <- sum(is.na(mydata))
total_missing
mydata <- na.omit(mydata)

corr1 <- corr_coef(mydata)
plot(corr1)
print(corr1)
getwd()
sink("corr1.txt")
print(corr1)
sink()

attach(mydata)
Pearson <- plot_cor(Academic.Pressure, Depression,
                    method="pearson",
                    ylab="Depression",
                    xlab="Academic Pressure",
                    theme=theme_classic(),
                    pointsize=2,
                    shape=20,
                    fill="black",
                    color="red",
                    axis.size=12,
                    ic=TRUE,
                    title = "Correlation Using Pearson")

Spearman <- plot_cor(Work.Pressure, Depression,
                     method="spearman",
                     ylab="Depression",
                     xlab="Work Pressure",
                     theme=theme_classic(),
                     pointsize=2,
                     shape=20,
                     fill="black",
                     color="red",
                     axis.size=12,
                     ic=TRUE,
                     title = "Correlation Using Spearman")

plot_list <- list() 
variables <- c("Academic.Pressure", 
               "Work.Pressure", 
               "CGPA", 
               "Study.Satisfaction", 
               "Job.Satisfaction", 
               "Work.Study.Hours", 
               "Financial.Stress")

for (var in variables)
{
  plot_list[[var]] <- ggplot(mydata, aes_string(x = "Depression", y = var)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "red") + 
    labs(title = paste(var, "vs Depression"), x = "Depression", y = var) + 
    theme_minimal()
}

for (i in 1:length(plot_list)) {
  print(plot_list[[i]])
}

class(mydata$Gender)
unique(mydata$Gender)
mydata$Gender <- factor(mydata$Gender, levels=c("Male", "Female"))
str(mydata$Gender)

ggplot(mydata, aes(x=Gender, y=Depression)) +
  geom_boxplot()

ggplot(mydata, aes(x=Depression)) +
  geom_histogram() + 
  facet_wrap(~Gender, ncol=1)

corr3 <- aov(Depression ~ Gender, data=mydata)
summary(corr3)

mydata <- mutate(mydata, newprice = cut(Depression,
                                        breaks=3,
                                        labels=c("Low", "Medium", "High")))
chidata <- mydata[, c("Gender", "newprice")]
chidata
table(chidata)
chisq.test(table(chidata), simulate.p.value = TRUE)
result <- chisq.test(table(chidata), simulate.p.value = TRUE)
result$expected

mydata <- mutate(mydata, newprice = cut(Depression, breaks=3, labels=c("Low", "Medium", "High")))
chidata <- mydata[, c("Gender", "newprice")]

table(chidata$newprice)

chidata_count <- chidata %>%
  count(Gender, newprice)

chidata_complete <- expand_grid(Gender = unique(chidata$Gender), newprice = unique(chidata$newprice)) %>%
  left_join(chidata_count, by = c("Gender", "newprice"))

head(chidata_complete)

ggplot(chidata_complete, aes(x = newprice, fill = Gender)) +
  geom_bar(aes(weight = n), position = "dodge") +
  labs(title = "Depression Category vs Gender",
       x = "Depression Category",
       y = "Count",
       fill = "Gender") +
  theme_minimal()
