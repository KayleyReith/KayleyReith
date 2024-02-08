#Kayley Reith
#COSC 3570 Final Script
#Wildfires in National Parks
#12/8/23

#packages needed for download 
rm(list=ls())
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(caret)
library(rpart.plot)
library(forecast)


data1 <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "NEW")
View(data1)


#PART 1 - EXPLORARTORY VIZUALIZATIONS
#Viz 1
#dev.off()
str(data1)
sapply(data1$Fire_Frequency, class)
data1$Fire_Frequency <- as.numeric(data1$Fire_Frequency)
summary_data <- data1 %>%
  group_by(Year, Region) %>%
  summarise(Avg_Fire_Frequency = mean(Fire_Frequency, na.rm = TRUE))
summary_data %>%
  ggplot(aes(x = Year, y = Avg_Fire_Frequency, color = Region)) +
  geom_point() +
  labs(title = "Average Fire Frequency Over Time by Region",
       x = "Year",
       y = "Average Fire Frequency",
       color = "Region") +
  theme_minimal()


#Viz2
data1 %>%
  ggplot(aes(x = Region, y = Fire_Frequency, fill = Region)) +
  geom_boxplot() +
  labs(title = "Distribution of Fire Frequencies by Region",
       x = "Region",
       y = "Fire Frequency",
       fill = "Region") +
  theme_minimal()


#Viz3
# Compare frequency of Human and Lightning fire types by region
df_filtered <- data1 %>%
  filter(Fire_Type %in% c("Human", "Lightning"))
ggplot(df_filtered, aes(x = Year, y = Fire_Frequency, color = Region)) +
  geom_line() +
  labs(x = "Year", y = "Fire Frequency") +
  ggtitle("Fire Frequency Comparison: Human vs. Lightning") +
  scale_color_discrete() +
  theme_minimal() +
  facet_wrap(~ Fire_Type, scales = "free_y", ncol = 2)

#Highlight southern and eastern area regions as "outliers" and need further investigation 

#Viz4
data2 <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "Supp Cost")
filtered_data <- data2 %>%
  filter(Year >= 2013 & Year <= 2022) #last 10 years 
data <- filtered_data %>%
  mutate(Total = as.numeric(gsub("\\$|,", "", Total))) %>% # Convert Total to numeric
  group_by(Year) %>%
  summarise(
    Total_Fires = sum(Fires),
    Total_Cost = sum(Total))

plot <- data %>%
  ggplot(aes(x = as.factor(Year), y = Total_Fires, label = scales::comma(Total_Cost))) +
  geom_col(fill = "skyblue", alpha = 0.7) +
  geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3) +
  labs(
    x = "Year",
    y = "Total Fires",
    title = "Exploratory Visualization: Fires and Total Cost in the Last 10 Years",
    subtitle = "Total Fires with Total Cost Labels",
    caption = "Note: Total Cost values are labeled on the bars") +
    theme_minimal()
print(plot)



#Viz5
data3 <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "Yosemite Visitors")
data3 <- data3 %>%
  na.omit() %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  group_by(Month) %>%
  summarise(Avg_Recreation_Visitors = mean(`Recreation Visitors`),
            Avg_Nonrecreation_Visitors = mean(`Nonrecreation Visitors`))
plot2 <- data3 %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Avg_Recreation_Visitors, color = "Recreation"), size = 1.5) +
  geom_line(aes(y = Avg_Nonrecreation_Visitors, color = "Nonrecreation"), size = 1.5) +
  geom_point(aes(y = Avg_Recreation_Visitors, color = "Recreation"), size = 3) +
  geom_point(aes(y = Avg_Nonrecreation_Visitors, color = "Nonrecreation"), size = 3) +
  scale_color_manual(values = c("Recreation" = "blue", "Nonrecreation" = "green")) +
  scale_y_continuous(labels = scales::number_format(scale = 1e6, suffix = "M")) +  
  labs(
    x = "Month",
    y = "Average Visitors",
    title = "Exploratory Visualization: Average Yosemite Visitors by Month",
    subtitle = "Average Recreation and Nonrecreation Visitors Trends",
    caption = "Note: Points represent the average visitors, lines connect the points"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(plot2)


#Viz6
data4 <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "Sequoia Visitors")
data4 <- data4 %>%
  na.omit() %>%
  mutate(Month = factor(Month, levels = month.name)) %>%
  group_by(Month) %>%
  summarise(Avg_Recreation_Visitors = mean(`Recreation Visitors`),
            Avg_Nonrecreation_Visitors = mean(`Nonrecreation Visitors`))
plot3 <- data4 %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Avg_Recreation_Visitors, color = "Recreation"), size = 1.5) +
  geom_line(aes(y = Avg_Nonrecreation_Visitors, color = "Nonrecreation"), size = 1.5) +
  geom_point(aes(y = Avg_Recreation_Visitors, color = "Recreation"), size = 3) +
  geom_point(aes(y = Avg_Nonrecreation_Visitors, color = "Nonrecreation"), size = 3) +
  scale_color_manual(values = c("Recreation" = "pink", "Nonrecreation" = "red")) +
  scale_y_continuous(labels = scales::number_format(scale = 1e6, suffix = "M")) +  
  labs(
    x = "Month",
    y = "Average Visitors",
    title = "Exploratory Visualization: Average Yosemite Visitors by Month",
    subtitle = "Average Recreation and Nonrecreation Visitors Trends",
    caption = "Note: Points represent the average visitors, lines connect the points"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(plot3)

#Exploratory Visuals 5 and 6 were for the old limited data of national park attendence and will not be used in the rest of the study 

#Viz7
Statedata <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "State")
Northern_Rockies <- c("Montana", "North Dakota")
Southwest <- c("New Mexico", "Arizona")
Great_Basin <- c("Nevada", "Utah", "Idaho")
California <- c("California", "Hawaii")
Northwest <- c("Oregon", "Washington")
Alaska <- c("Alaska")
Rocky_Mountains <- c("Colorado", "Kansas", "Nebraska", "Wyoming", "South Dakota")
Southern_Area <- c("Texas", "Oklahoma", "Louisiana", "Mississippi", "Georgia", 
                   "Florida", "Tennessee", "Kentucky", "Virginia", "South Carolina", 
                   "North Carolina", "Arkansas", "Alabama")
Eastern_Area <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
                  "Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware", 
                  "Maryland", "West Virginia", "Ohio", "Michigan", "Indiana", "Illinois", 
                  "Wisconsin", "Minnesota", "Iowa")


Statedata$Region <- NA
Statedata$Region[Statedata$State %in% Northern_Rockies] <- "Northern Rockies"
Statedata$Region[Statedata$State %in% Southwest] <- "Southwest"
Statedata$Region[Statedata$State %in% Great_Basin] <- "Great Basin"
Statedata$Region[Statedata$State %in% Northwest] <- "Northwest"
Statedata$Region[Statedata$State %in% Alaska] <- "Alaska"
Statedata$Region[Statedata$State %in% Rocky_Mountains] <- "Rocky Mountains"
Statedata$Region[Statedata$State %in% Southern_Area] <- "Southern Area"
Statedata$Region[Statedata$State %in% Eastern_Area] <- "Eastern Area"
Statedata <- Statedata %>%
  mutate(Region = case_when(
    State %in% Northern_Rockies ~ "Northern Rockies",
    State %in% Southwest ~ "Southwest",
    State %in% Great_Basin ~ "Great Basin",
    State %in% Northwest ~ "Northwest",
    State %in% Alaska ~ "Alaska",
    State %in% Rocky_Mountains ~ "Rocky Mountains",
    State %in% Southern_Area ~ "Southern Area",
    State %in% Eastern_Area ~ "Eastern Area",
    State %in% California ~ "California",
    TRUE ~ "Other"))
View(Statedata)
# Bar plot for Fires Total by Region
Statedata %>%
  group_by(Region, Year) %>%
  summarize(TotalFires = sum(`Fires Total`)) %>%
  ggplot(aes(x = Year, y = TotalFires, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Fires Over the Years by Region",
       x = "Year", y = "Total Number of Fires", fill = "Region") +
  theme_minimal()


total_fires_by_region <- Statedata %>%
  group_by(Region) %>%
  summarize(TotalFires = sum(`Fires Total`))
max_region <- total_fires_by_region %>%
  slice(which.max(TotalFires))
ggplot(total_fires_by_region, aes(x = reorder(Region, -TotalFires), y = TotalFires)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Fires by Region",
       x = "Region", y = "Total Number of Fires") +
  theme_minimal() +
  geom_text(aes(label = TotalFires), vjust = -0.5, size = 4, color = "darkred") +
  annotate("text", x = max_region$Region, y = max_region$TotalFires + 2000,
           label = paste("Max Region:", max_region$Region, "\n", "Total Fires:", max_region$TotalFires),
           color = "darkred", hjust = 0.5)


#Viz9 - State fires by Region
Southern_Area_data <- Statedata %>%
  filter(State %in% Southern_Area)
ggplot(Southern_Area_data, aes(x = State, y = `Fires Total`, fill = `Fires Total`)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Total Fires by State in the Southern Area",
       x = "State", y = "Total Number of Fires", fill = "Fires Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = `Fires Total`), vjust = -0.5, size = 3, color = "darkred")
#Texas has the greatest wildfires 







#PART 2 - CANDIDATE TECHNIQUES 

#PAIRWISE VIZUALIZATION
install.packages("psych")
library(psych)
numeric_vars <- Statedata[, sapply(Statedata, is.numeric)]
#pairs(Statedata)
pairs(numeric_vars)
#independent variables 
selected_vars <- c("Chemical Exports(Millions)", "Unemployment Rates", "Percentage of Obese People", "State", "Fires Total")
subset_data <- Statedata[selected_vars]
pairs(subset_data)
summary(subset_data)
# There is a strong positive correlation between Fires Total and Unemployment Rates. 
#This suggests that states with higher unemployment rates also tend to have more fires.
#There is a moderate positive correlation between Fires Total and Chemical Exports (Millions). 
#This suggests that states with higher levels of chemical exports also tend to have more fires.


#LINEAR REGRESSION
#simple linear regression 
Statedata <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "State")
View(Statedata)
lm1 <- lm(`Fires Total` ~ `Chemical Exports(Millions)`, data = Statedata)
summary(lm1)
anova_table <- anova(lm1)
print(anova_table)
#The linear regression model predicts Fires Total based on Chemical Exports(Millions). 
#The intercept is 740.78, representing the estimated Fires Total when chemical exports are zero. 
#For each additional million in chemical exports, Fires Total is predicted to increase by 0.19 units. 
#The model is statistically significant (p < 0.05), explaining 48.32% of the variability in Fires Total

#MLR Statewide data
lm2<- lm(`Fires Total` ~ `Chemical Exports(Millions)` + `Unemployment Rates`, data = Statedata)
summary(lm2)
anova_table <- anova(lm2)
print(anova_table)

dev.off()
ggplot(Southern_Area_data, aes(x = `Chemical Exports(Millions)`, y = `Fires Total`)) +
  geom_point(aes(color = `Unemployment Rates`)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  labs(title = "Linear Regression: Fires Total vs. Chemical Exports & Unemployment Rates (Southern Area)",
       x = "Chemical Exports(Millions)", y = "Fires Total") +
  theme_minimal()
#The multiple linear regression model with predictor Chemical Exports(Millions), exhibits statistical significance, with a low p-value (p = 0.000215). 
#The positive coefficient for Chemical Exports(Millions) (0.1836) suggests that an increase in chemical exports is associated with a predicted increase in wildfires. 
#The model's high R squared value of 0.7264 indicates that approximately 72.64% of the variability in Fires Total is explained by the model. 

lm<-lm(`Fires Total` ~ `Chemical Exports(Millions)` + `Acres Total` + `Unemployment Rates`, data = Southern_Area_data)
summary(lm)
anova_table <- anova(lm)
print(anova_table)

#The intercept is not statistically significant (p = 0.4862), suggesting that the model might not have a meaningful starting point. 
#The positive coefficient for Chemical Exports(Millions) (0.1495) is marginally significant (p = 0.0521), indicating a potential association with increased wildfires.
#The overall model explains a substantial 76% proportion of the variability in Fires Total. 

library(ggplot2)
library(broom)
lm_model <- lm(`Fires Total` ~ `Chemical Exports(Millions)` + `Acres Total` + `Unemployment Rates`, data = Southern_Area_data)
summary(lm_model)
tidy_data <- tidy(lm_model)
#  95% confidence interval
ggplot(Southern_Area_data, aes(x = `Chemical Exports(Millions)`, y = `Fires Total`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", formula = y ~ x) +
  labs(title = "Linear Regression with 95% Confidence Interval",
       x = "Chemical Exports(Millions)",
       y = "Fires Total") +
  theme_minimal()



pairs(Southern_Area_data[, c("Fires Total", "Chemical Exports(Millions)", "Acres Total", "Unemployment Rates")],
      main = "Pair Plot of Predictors based on Multiple Linear Regression",
      pch = 16, col = Southern_Area_data$`Unemployment Rates`)
par(mfrow = c(2, 2))  
# 1. Residuals vs. Fitted Values Plot
plot(lm_model, which = 1)
#There is a spike and cluster of data points indicating their may be a different type of realtionship between fires and chemical exports, unemployment rates, and total acreage that is not linear. 
plot(lm_model, which = 2)
# Normal Q-Q Plot - plot is distributed normally 
plot(lm_model, which = 3)
# Scale-Location Plot (Square root of standardized residuals vs. fitted values)
#The distribution of points in nonconstant and suggest heterscedasticity 
plot(lm_model, which = 4)
# Cook's Distance Plot


#Variable Selection for linear regression best models 
library(readxl)
library(olsrr) #Source: Dr.Yu MATH 4780 Lecture Slides 
Statedata <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "State")
Statedata$State <- as.numeric(factor(Statedata$State))
Statedata$Region <- as.numeric(factor(Statedata$Region))
Statedata$Human_Fires <- Statedata$`Human Fires`
Statedata$Fires_Total <- Statedata$`Fires Total`
Statedata$Lightning_Fires <- Statedata$`Lightning Fires`


lm_full <- lm(Human_Fires ~ Year + Region  + `Human Acres` + `Fires Total`+
                `Lightning Fires` + `Lightning Acres` + `Acres Total` +
                `Unemployment Rates` + `Chemical Exports(Millions)` +
                `Percentage of Obese People`, data = Statedata)
summary_lm <- summary(lm_full)
#StepWise Regression #Source: Dr.Yu MATH 4780 Lecture Slides 
olsrr_all <- olsrr::ols_step_all_possible(lm_full) #Source: Dr.Yu MATH 4780 Lecture Slides (lines 364-390)
stepwise_results <- olsrr_all$steps$ols_steps
print(stepwise_results[, c("Index", "Predictors", "R-Square", "Adj. R-Square", "Cp")]) #Source - Dr.Yu's Lecture Slides (MATH 4780)
olsrr::ols_step_best_subset(lm_full, metric = "predrsq")
summary_lm <- summary(lm_full)
#Model 5 is the strongest candidate as it has a high Adjusted R-Square (0.9909) and relatively low Mallow's Cp (3.8733) compared to other models. 
#Model 5 consists of predictors: `Human Acres`,  `Fires Total`,  `Lightning Fires`, `Lightning Acres`, and Acres Total`   
bestlm1 <- lm(Human_Fires ~ `Human Acres` + `Fires Total`+
                `Lightning Fires` + `Lightning Acres` + `Acres Total`, data = Statedata)
summary(bestlm1)
summary_table <- coef(summary(bestlm1))
print(summary_table)
#Residuals vs. Fitted plot
plot(fitted(bestlm1), residuals(bestlm1), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)
#  Normal Q-Q plot
qqnorm(residuals(bestlm1))
qqline(residuals(bestlm1), col = "red")
# Scale-Location plot
plot(fitted(bestlm1), sqrt(abs(residuals(bestlm1)/sd(residuals(bestlm1)))),
     xlab = "Fitted values", ylab = "√|Standardized Residuals|",
     main = "Scale-Location Plot")
# Scale-Location plot
plot(fitted(bestlm1), sqrt(abs(residuals(bestlm1)/sd(residuals(bestlm1)))),
     xlab = "Fitted values", ylab = "√|Standardized Residuals|",
     main = "Scale-Location Plot")



lm_full2 <- lm(Lightning_Fires ~ Year + Region  + `Human Acres` + `Fires Total`+
                 `Lightning Acres` + `Acres Total` +
                 `Unemployment Rates` + `Chemical Exports(Millions)` +
                 `Percentage of Obese People`, data = Statedata)
olsrr_all <- olsrr::ols_step_all_possible(lm_full2)
stepwise_results <- olsrr_all$steps$ols_steps
print(stepwise_results[, c("Index", "Predictors", "R-Square", "Adj. R-Square", "Cp")]) #Source - Dr.Yu's Lecture Slides (MATH 4780)
olsrr::ols_step_best_subset(lm_full2, metric = "predrsq")
#Model 5 has the best balance of these criteria, with a  high adjusted R-squared (0.3665) and lower MSEP and Mallows CP compared to other models.
##Model 5 consists of predictors: Region,  `Fires Total`,  `Acres Total`,  `Chemical Exports(Millions)`, and  `Percentage of Obese People`                                                           

bestlm2<- lm(Lightning_Fires ~ Region +`Fires Total` +`Acres Total` +`Chemical Exports(Millions)` +`Percentage of Obese People`, data=Statedata)
summary(bestlm2)
summary_table <- coef(summary(bestlm2))
print(summary_table)

plot(fitted(bestlm1), residuals(bestlm1), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)
#  Normal Q-Q plot
qqnorm(residuals(bestlm2))
qqline(residuals(bestlm2), col = "red")
# Scale-Location plot
plot(fitted(bestlm1), sqrt(abs(residuals(bestlm2)/sd(residuals(bestlm2)))),
     xlab = "Fitted values", ylab = "√|Standardized Residuals|",
     main = "Scale-Location Plot")
# Scale-Location plot
plot(fitted(bestlm2), sqrt(abs(residuals(bestlm2)/sd(residuals(bestlm2)))),
     xlab = "Fitted values", ylab = "√|Standardized Residuals|",
     main = "Scale-Location Plot")





#MLR - Focus on Eastern Region
Eastern_Area_data <- Statedata %>%
  filter(State %in% Eastern_Area)
lm4 <- lm(`Fires Total` ~ `Chemical Exports(Millions)` + `Unemployment Rates`, data = Eastern_Area_data)
summary(lm4)
anova_table <- anova(lm4)
print(anova_table)



#Mutate regions as variables 
Alaska_data <- Statedata %>%
  filter(State %in% Alaska)
Eastern_Area_data <- Statedata %>%
  filter(State %in% Eastern_Area)
Great_Basin_Area_data <- Statedata %>%
  filter(State %in% Great_Basin)
Northern_California_data <- Statedata %>% 
  filter(Region == "Northern California")
Northern_Rockies_data <- Statedata %>%
  filter(State %in% Northern_Rockies)
Northwest_Area_data <- Statedata %>%
  filter(State %in% Northwest)
Rocky_Mountains_Area_data <- Statedata %>%
  filter(State %in% Rocky_Mountains)
Southern_Area_data <- Statedata %>%
  filter(State %in% Southern_Area)
Southern_California_Area_data <- Statedata %>%
  filter(State %in% "Southern California")
Southwest_Area_data <- Statedata %>%
  filter(State %in% Southwest)
Western_Great_Basin_data <- Statedata %>%
  filter(State %in% "Western Great Basin")


#The commented out linear regression equations below yield errors or results with insignificant results so they were excluded from the model 

#lm1 <- lm(`Lightning Fires` ~  +`Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Alaska_data)
#summary(lm1)

lm2 <- lm(`Lightning Fires` ~ +`Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Eastern_Area_data)
summary(lm2)

#lm3 <- lm(`Lightning Fires` ~  `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Great_Basin_Area_data)
#summary(lm3)

#lm4 <- lm(`Lightning Fires` ~  `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Northern_California_data)
#summary(lm4)

#lm5 <- lm(`Lightning Fires` ~ `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Northwest_Area_data)
#summary(lm5)

#lm6 <- lm(`Lightning Fires` ~  `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Rocky_Mountains_Area_data)
#summary(lm6)

lm7 <- lm(`Lightning Fires` ~ `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Southern_Area_data)
summary(lm7)

#lm8 <- lm(`Lightning Fires` ~  `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Southern_California_Area_data)
#summary(lm8)

#lm9 <- lm(`Lightning Fires` ~ `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Southwest_Area_data)
#summary(lm9)

#lm10 <- lm(`Lightning Fires` ~ `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Western_Great_Basin_data)
#summary(lm10)

#Linear regression equations 1, 3, 4, 5, 6 , 8, 9, 10 are deterministic, multicollinear or achieved errors. 
#This indicates how model 2 and 7 are the only somewhat significant and meaningful models 
#These two models contain the regions Southern Area and Eastern Area which where the 2 highest regions with wildfire frequency. specifically with lightning made fires

#Same model but using all regions instead of partitioning them 
lm11 <- lm(`Lightning Fires` ~ `Fires Total`+ `Acres Total` + `Chemical Exports(Millions)`+ `Percentage of Obese People`, data = Statedata)
summary(lm11)



#Logistic Regression - statewide
Statedata <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "State")
Statedata$Region <- as.factor(Statedata$Region)

logistic_model <- glm(Region ~ `Fires Total` + `Chemical Exports(Millions)` + 
                        `Unemployment Rates` + `Percentage of Obese People`, 
                      data = Statedata, family = "binomial")

summary(logistic_model)
library(rpart)
library(rpart.plot)
library(partykit)
Statedata$Region <- as.factor(Statedata$Region)
classification_tree <- rpart(Region ~ + `Chemical Exports(Millions)` + 
                               `Unemployment Rates` + `Percentage of Obese People`, 
                             data = Statedata, method = "class")
prp(classification_tree, type = 1, extra = 1, under = TRUE)


classification_tree <- rpart(`State` ~ `Chemical Exports(Millions)` + 
                               `Unemployment Rates` + `Percentage of Obese People`, 
                             data = Statedata, method = "class")
prp(classification_tree, type = 1, extra = 1, under = TRUE)
party_obj <- as.party(classification_tree)
plot(party_obj, type = "simple")



#LOGISTIC REGRESSION

Statedata$Binary_Human_Fires <- ifelse(Statedata$`Human Fires` > 0, 1, 0)
Logistic1 <- glm(Binary_Human_Fires ~ `Human Acres` + `Chemical Exports(Millions)`, family = binomial(link = logit), data = Statedata)
summary(Logistic1)
#The logistic regression model suggests that the variables 'Human Acres' and 'Chemical Exports(Millions)' have coefficients close to zero.
#This indicates having little individual impact on predicting 'Binary_Human_Fires.' 
#The overall model shows a strong fit to the data with low deviance residuals. 
#There is still a chance for small contribution of 'Human Acres' and 'Chemical Exports(Millions)' to influence explaining the occurrence of human-caused fires.



#Logitic regression testing southern area vs other regions 
Statedata$Binary_Region_Southern_Area <- ifelse(Statedata$Region == "Southern_Area", 1, 0)
Logistic2 <- glm(Binary_Region_Southern_Area ~ `Fires Total` + `Chemical Exports(Millions)` + `Unemployment Rates` + `Percentage of Obese People`, 
                 family = binomial(link = logit), data = Statedata)
summary(Logistic2)
#this result was inconclusive as there were no statisically significant results from the logisitic regression specific to the southern area region 


#CLASSIFICATION TREE
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(rpart.plot)
install.packages("partykit")
library(partykit) # Source: R/decision_tree_partykit.R https://parsnip.tidymodels.org/reference/details_decision_tree_partykit.html 
Statedata <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "State")
Statedata$Region <- as.factor(Statedata$Region)
Statedata$State <- as.factor(Statedata$State)
set.seed(1)
myIndex <- createDataPartition(Statedata$Region, p=0.7, list=FALSE) #70% training, 30% validation
trainSet <- Statedata[myIndex,]
validationSet <- Statedata[-myIndex,]
set.seed(1)
default_tree <- rpart(Region ~ `Human Acres` + `Lightning Acres` + `Unemployment Rates` + `Chemical Exports(Millions)`,  
                      data = trainSet, 
                      method = "class")

summary(default_tree)
prp(default_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)
party_obj <- as.party(default_tree)
plot(party_obj, type = "simple")



set.seed(1)
default_tree1 <- rpart(`Region` ~  `Chemical Exports(Millions)` + `Percentage of Obese People`, 
                      data = trainSet, 
                      method = "class")
summary(default_tree1)
prp(default_tree1, 
    type = 1, 
    extra = 1, 
    under = TRUE)
party_obj <- as.party(default_tree1)
plot(party_obj, type = "simple")



#CLASSIFICATION TREE - testing states at risk for wildfires 
set.seed(1)
myIndex <- createDataPartition(Statedata$State, p=0.7, list=FALSE) #70% training, 30% validation
trainSet <- Statedata[myIndex,]
validationSet <- Statedata[-myIndex,]
set.seed(1)

default_tree2 <- rpart(State ~ `Unemployment Rates` + `Chemical Exports(Millions)` + `Region`,  
                      data = trainSet, 
                      method = "class")

summary(default_tree2)
prp(default_tree2, 
    type = 1, 
    extra = 1, 
    under = TRUE)

library(rpart.plot)
install.packages("partykit")
library(partykit) # Source: R/decision_tree_partykit.R https://parsnip.tidymodels.org/reference/details_decision_tree_partykit.html 
party_obj <- as.party(default_tree2)
plot(party_obj, type = "simple")




#FULL CLASSIFICATION TREE

library(readxl)
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
Statedata <- read_excel("/Users/kayleyreith/Desktop/Book2.xlsx", sheet = "State")

set.seed(1)
myIndex <- createDataPartition(Statedata$State, p=0.7, list=FALSE) #70% training, 30% validation
trainSet <- Statedata[myIndex,]
validationSet <- Statedata[-myIndex,]
set.seed(1)


set.seed(1)
full_tree <- rpart(State ~ `Unemployment Rates` + `Chemical Exports(Millions)` + `Region`,  
      data = trainSet, 
      method = "class",
      cp = 0, 
      minsplit = 2, 
      minbucket = 1)


prp(full_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)


pruned_tree <- prune(full_tree, cp = 0.0068682)
prp(pruned_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)
print(pruned_tree)


library(partykit) # Source: R/decision_tree_partykit.R https://parsnip.tidymodels.org/reference/details_decision_tree_partykit.html 
party_obj <- as.party(pruned_tree)
plot(party_obj, type = "simple")


#This decision tree is almost impossible to read due to the nature of the large widespread data 
#It is difficult to interpret which states are the outcomes of each node







