#Kayley Reith
#MATH 4780 
#Final Project
#PREIDCT THE GRADES 50 AND BELOW OR 50 AND ABOVE AND SEE IF THEER IS ANY DIFFERENCES THERE 
rm(list=ls())
library(readxl)
data1 <- read.csv("/Users/kayleyreith/Desktop/student_math_clean.csv")
str(data1)
summary(data1)
head(data1)
View(data1)
#Final Grade = response


zero_values <- subset(data1, final_grade == 0)
print(zero_values)
View(zero_values)
half_values <- subset(data1, final_grade <= 10)
View(half_values)



lm1<- lm(final_grade ~ class_failures + free_time, data=data1)
summary(lm1)
alpha <- 0.05
SS_res <- sum(lm1$residuals ^ 2)
SS_res
low_bd <- SS_res / qchisq(alpha / 2, df = lm1$df, lower.tail = FALSE)
upp_bd <- SS_res / qchisq(alpha / 2, df = lm1$df, lower.tail = TRUE)
low_bd
upp_bd
anova_table <- anova(lm1)
anova_table
r_squared <- summary(lm1)$r.squared
percentage_variability <- r_squared * 100
cat(percentage_variability,"%")
#class absence grade 1 grade 2 influence 




head(data1)
data1[!complete.cases(data1)]

length(data1[data1$class_failures != 0,])
str(data1)

factor(data1$mother_job)

data1$mother_education <- as.factor(data1$mother_education)
data1$father_education <- as.factor(data1$father_education)
data1$extra_paid_classes <- as.factor(data1$extra_paid_classes)
data1$activities <- as.factor(data1$activities)
data1$study_time <- as.factor(data1$study_time)
data1$mother_job <- as.factor(data1$mother_job)
data1$father_job <- as.factor(data1$father_job)

data1 <- data1 %>%
  mutate(mother_4education = as.factor(ifelse(mother_education == 'primary education (4th grade)', 1, 0))) %>%
  mutate(mother_5to9education = as.factor(ifelse(mother_education == '5th to 9th grade', 1, 0))) %>%
  mutate(mother_secondaryeducation = as.factor(ifelse(mother_education == 'secondary education', 1, 0))) %>%
  mutate(mother_highereducation = as.factor(ifelse(mother_education == 'higher education', 1, 0))) %>%
  mutate(father_4education = as.factor(ifelse(father_education == 'primary education (4th grade)', 1, 0))) %>%
  mutate(father_5to9education = as.factor(ifelse(father_education == '5th to 9th grade', 1, 0))) %>%
  mutate(father_secondaryeducation = as.factor(ifelse(father_education == 'secondary education', 1, 0))) %>%
  mutate(father_highereducation = as.factor(ifelse(father_education == 'higher education', 1, 0))) %>%
  mutate(extraPaidClasses = as.factor(ifelse(extra_paid_classes == 'yes', 1, 0))) %>%
  mutate(Activities = as.factor(ifelse(activities == 'yes', 1, 0))) %>%
  mutate(study_time2to5 = as.factor(ifelse(study_time  == '2 to 5 hours', 1, 0))) %>%
  mutate(study_time5to10 = as.factor(ifelse(study_time == '5 to 10 hours', 1, 0))) %>%
  mutate(study_time10more = as.factor(ifelse(study_time == '>10 hours', 1, 0)))

str(data1)

lm1 <- lm(final_grade ~ grade_1 + grade_2 + absences + class_failures + mother_4education + mother_5to9education + mother_secondaryeducation + mother_highereducation + father_4education + father_5to9education + father_secondaryeducation + father_highereducation + extraPaidClasses + Activities + study_time10more, data = data1)
summary(lm1)

lm1 <- lm(final_grade ~ grade_1 + grade_2 + absences + class_failures + mother_4education + mother_5to9education + mother_secondaryeducation + mother_highereducation + father_4education + father_5to9education + father_secondaryeducation + father_highereducation + extraPaidClasses + Activities + study_time10more, data = data1)
summary(lm1)
install.packages("olsrr")
library(olsrr)
olsrr_all <- olsrr:: ols_step_all_possible(lm1)
names(olsrr_all)
stepwise_results <- olsrr_all$steps$ols_steps
print(stepwise_results[, c("Index", "Predictors", "R-Square", "Adj. R-Square", "Cp")])
olsrr::ols_step_best_subset(lm_full, metric = "predrsq")


lm1 <- lm(final_grade ~ class_failures + free_time + age + mother_education, data = data1)
predictions <- predict(lm1, interval = "confidence", level = 0.95)
print(predictions)
library(olsrr)
olsrr_all <- olsrr:: ols_step_all_possible(lm1)
print(olsrr_all)
names(olsrr_all)
stepwise_results <- olsrr_all$steps$ols_steps
print(stepwise_results[, c("Index", "Predictors", "R-Square", "Adj. R-Square", "Cp")])
olsrr::ols_step_best_subset(lm1)


#MODEL 2
lm2 <- lm(final_grade ~ class_failures + absences + free_time, data = data1)
summary(lm2)
alpha <- 0.05
SS_res <- sum(lm2$residuals ^ 2)
SS_res
low_bd <- SS_res / qchisq(alpha / 2, df = lm2$df, lower.tail = FALSE)
upp_bd <- SS_res / qchisq(alpha / 2, df = lm2$df, lower.tail = TRUE)
low_bd
upp_bd


library(dplyr)

# Assuming 'data1' is your dataset
avggrade <- data1 %>%
  mutate(test_average = (grade_1 + grade_2) / 2)

# Print the modified data frame
print(avggrade)
View(avggrade)
lm3 <- lm(avggrade ~ class_failures + absences + free_time, data = data1)
summary(lm3)

