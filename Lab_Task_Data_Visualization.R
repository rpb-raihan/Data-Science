library(moments)
library(ggplot2) 



mydata <- read.csv("C:/AIUB/OneDrive - American International University-Bangladesh/9th Semester/Data Science/Final/Lab/student/Student Depression Dataset.csv", header = TRUE, sep = ",")

mydata 
View(mydata)


str(mydata)
colnames(mydata)


total_missing <- sum(is.na(mydata))
total_missing
#histogram
x <- mydata$CGPA
summary(x)  
table(x) 
mode_x <- as.numeric(names(sort(table(x), decreasing = TRUE)[1])) 
mode_x
hist_data <- hist(x, plot = FALSE, breaks=20)
hist(x,main="Histogram of CGPA", xlab="CGPA", ylab="Frequency", xlim=c(5,10), ylim=c(0,6000), col="pink", border="black") 
mean_x <- mean(x) 
sd_x <- sd(x)  
x_range <- seq(min(x), max(x), length = 1000)  
density_curve <- density(x) 
scaled_density <- density_curve$y * length(x) * diff(hist_data$breaks)[1] 
lines(density_curve$x, scaled_density, col = "darkblue", lwd = 2)  
lines(x_range, dnorm(x_range, mean = mean_x, sd = sd_x) * length(x) * diff(hist_data$breaks)[1], col = "red", lwd = 2, lty = 2)  
abline(v=mean(x), lwd=4, col="red")  
abline(v=median(x), lwd=4, col="blue")  
abline(v=mode_x, lwd=4, col="green")  
legend("topleft", legend = c("Mean", "Median", "Mode", "Histogram Density", "Normal Curve"), col = c("red", "blue", "green","darkblue", "red"), lwd = 1,pch=16, cex=.75) 

#bar
table(mydata$Degree)
ggplot(mydata, aes(x = Degree)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  
  labs(title = "Bar Graph of Degree", x = "Degree", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#boxplot
summary(mydata$CGPA)
ggplot(data = mydata, aes(x = "", y = CGPA)) +
  geom_boxplot(fill = "#EAB595", color = "black") +
  labs(title = "Boxplot of CGPA", x = "", y = "CGPA") +
  theme_minimal()
findoutliers <- boxplot.stats(mydata$CGPA)$out
findoutliers

summary(mydata$Age)
ggplot(data = mydata, aes(x = "", y = Age)) +
  geom_boxplot(fill = "#D87F81", color = "#EAB595") +
  labs(title = "Boxplot of Age", x = "", y = "Age") +
  theme_minimal()
findoutliers <- boxplot.stats(mydata$Age)$out
findoutliers


#scatter
ggplot(mydata, aes(x = CGPA, y = Age)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot of CGPA vs Age with Trend Line", x = "CGPA", y = "Age") +
  theme_minimal()

#Violin
class(mydata$Depression)
unique(mydata$Depression)
mydata$Depression <- factor(mydata$Depression, levels = c("0", "1"))
str(mydata$Depression)

ggplot(mydata, aes(x = Depression, y = CGPA, fill = Depression)) + 
  geom_violin(trim = FALSE, size = 1.5, alpha = 0.7) + 
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", size = 0.5) + 
  stat_summary(fun = median, geom = "point", size = 2, color = "red") + 
  theme_minimal() +
  labs(title = "Violin Plot", 
       x = "Depression",
       y = "CGPA") +
  scale_fill_brewer(palette = "Set3") 



mydata$Age_cat <- cut(mydata$Age, breaks = 3, labels = c("Young adults", "Middle aged", "Old aged"))

ggplot(mydata, aes(x = Age_cat, y = Work.Study.Hours, fill = Age_cat)) + 
  geom_violin(trim = FALSE, size = 1.5, alpha = 0.7) + 
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", size = 0.5) + 
  stat_summary(fun = median, geom = "point", size = 2, color = "red") + 
  theme_minimal() +
  labs(title = "Violin Plot",
       x = "Age",
       y = "Work.Study.Hours") +
  scale_fill_brewer(palette = "Set3") 

#line
line<-ggplot(mydata, aes(x=Dietary.Habits, y=Age, group=1)) +
  geom_line(color="maroon",size=1)+
  geom_point()

line+ggtitle("Dietary Habits vs Age")+
  labs(x="Dietary Habits",y="Age")



