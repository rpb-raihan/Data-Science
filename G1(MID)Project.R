dataSet<-read.csv('D:/4th year varsity/Introduction to data science/Dataset(Updated)_MIdterm_sectoin(F) (1).csv',header = TRUE,sep = ",")
print(dataSet)

library(dplyr)
library(VIM)


dataSet$gender[dataSet$gender == ""] <- NA
dataSet$smoking_history[dataSet$smoking_history == ""] <- NA
colSums(is.na(dataSet))
aggr(dataSet, numbers = TRUE, sortVars = TRUE, cex.axis = .7, gap = 3,
     ylab = c("Missing Data", "Pattern"))



duplicated_rows <- dataSet[duplicated(dataSet), ]
print(duplicated_rows)
dataSet<-distinct(dataSet)



colSums(is.na(dataSet))
gender_mode <- names(which.max(table(dataSet$gender)))
dataSet <- mutate(dataSet, gender = ifelse(is.na(gender), gender_mode, gender))

smoking_history_mode <- names(which.max(table(dataSet$smoking_history, useNA = "no")))
print(smoking_history_mode)
dataSet <- mutate(dataSet, smoking_history = ifelse(is.na(smoking_history),smoking_history_mode, smoking_history))
dataSet <- mutate(dataSet,age = abs(age),bmi = abs(bmi))

median_age <- summarise(dataSet, median_age = median(age, na.rm = TRUE))
median_age <- pull(median_age, median_age)
dataSet <- mutate(dataSet, age = ifelse(is.na(age), median_age, age))

mean_bmi <- summarise(dataSet, mean_bmi = mean(bmi, na.rm = TRUE))
mean_bmi <- pull(mean_bmi, mean_bmi)
dataSet <- mutate(dataSet, bmi = ifelse(is.na(bmi), mean_bmi, bmi))

hypertension_mode <- names(which.max(table(dataSet$hypertension)))
dataSet <- mutate(dataSet, hypertension = ifelse(is.na(hypertension), hypertension_mode, hypertension))
age_bounds <- summarise(dataSet, 
                        Q1 = quantile(age, 0.25, na.rm = TRUE),
                        Q3 = quantile(age, 0.75, na.rm = TRUE),
                        IQR = Q3 - Q1,
                        lower = Q1 - 1.5 * IQR,
                        upper = Q3 + 1.5 * IQR
)
clean_data <- filter(dataSet, age >= age_bounds$lower & age <= age_bounds$upper)

clean_data <- mutate(dataSet,
                     blood_glucose_level = as.numeric(gsub("[^0-9.]", "", blood_glucose_level))
)
clean_data <- mutate(clean_data,
                     gender = case_when(
                       grepl("^F", gender, ignore.case = TRUE) ~ "Female",
                       grepl("^M", gender, ignore.case = TRUE) ~ "Male",
                     )
)


clean_data$gender <- factor(clean_data$gender,
                         levels = c("Male", "Female"),
                         labels = c(0, 1))

clean_data$hypertension <- factor(clean_data$hypertension,
                               levels = c(0, 1),
                               labels = c("No", "Yes"))
clean_data$heart_disease <- factor(clean_data$heart_disease,
                                levels = c(0, 1),
                                labels = c("No", "Yes"))
clean_data$smoking_history <- tolower(clean_data$smoking_history)
unique(clean_data$smoking_history)
clean_data$smoking_history <- factor(clean_data$smoking_history,
                                  levels = c("never", "no info", "current", "former", "ever", "not current"),
                                  labels = c(1, 2, 3, 4, 5, 6))

clean_data$diabetes <- factor(clean_data$diabetes,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))

min_glucose <- min(clean_data$blood_glucose_level, na.rm = TRUE)
max_glucose <- max(clean_data$blood_glucose_level, na.rm = TRUE)
clean_data <- mutate(clean_data,glucose_normalized = (blood_glucose_level - min_glucose) / (max_glucose - min_glucose))
clean_data

filtered_byage <- filter(clean_data, age > 60)
print(filtered_byage)
filtered_dataBMI <- filter(clean_data, bmi >= 18.5 & bmi <= 24.9)
print(filtered_dataBMI)
filtered_complex <- filter(clean_data,gender == "0" & age >= 20 & age <= 60 & smoking_history %in% c("5", "4") & heart_disease == 'No')
print(filtered_complex)




balance_data <- function(clean_data, target_col) {
  class_counts <- pull(count(clean_data, {{target_col}}), n)
  minority_class <- pull(slice_min(count(clean_data, {{target_col}}), n), {{target_col}})
  majority_class <- pull(slice_max(count(clean_data, {{target_col}}), n), {{target_col}})
  balanced_data <- ungroup(sample_n(group_by(clean_data, {{target_col}}), size = min(class_counts)))
  return(balanced_data)
}
balanced_dataset <- balance_data(clean_data, diabetes)
print(balanced_dataset,n=Inf)
set.seed(123)
sample_indices <- sample(1:nrow(clean_data), size = 0.7 * nrow(clean_data))
train_data <- clean_data[sample_indices, ]
test_data <- clean_data[-sample_indices, ]

age_stats <- arrange(
  summarise(
    group_by(clean_data, gender),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = {
      tbl <- table(age)
      as.numeric(names(tbl)[which.max(tbl)])
    },
    count = n()
  ),
  gender
)
print(age_stats)

age_stats <- arrange(
  summarise(
    group_by(clean_data, hypertension),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    mode_age = {
      tbl <- table(age)
      as.numeric(names(tbl)[which.max(tbl)])
    },
    count = n()
  ),
  hypertension
)
print(age_stats)
age_spread_stats <- arrange(
  summarise(
    group_by(clean_data, gender),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    range = max_age - min_age,
    Q1 = quantile(age, 0.25, na.rm = TRUE),
    Q3 = quantile(age, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    variance = var(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    count = n()
  ),
  gender
)
print(age_spread_stats)

