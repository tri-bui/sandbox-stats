### Dependencies and Data ###

# Libraries
# install.packages('tidyverse')
library(tidyverse)

# Used car data
used_car_data <- read.csv(file = 'data/used_car_data.csv', 
                          stringsAsFactors = F)

# MPG data
mpg_data <- read.csv(file = 'data/mpg_data.csv', 
                     stringsAsFactors = F)
mpg99 <- mpg_data %>% filter(year == 1999)
mpg08 <- mpg_data %>% filter(year == 2008)

# Built-in cars data
car_data <- mtcars


### Tests for Normality ###

# Qualitative test: density plot
ggplot(used_car_data, aes(x = Miles_Driven)) + 
  geom_density()

# Qualitative test: Shapiro-Wilk test
shapiro.test(used_car_data$Miles_Driven)

## Since the p-value < 0.05, the Miles_Driven variable is 
## not considered  normally distributed.


### Log Transformation ###

# Plot again with a log-transform
ggplot(used_car_data, aes(log10(Miles_Driven))) + 
  geom_density()

## After a log transformation, the right-skewed data now 
## looks approximately normal.


### Random Sampling ###

# Create 2 random samples of the data
used_car_samp1 <- used_car_data %>% sample_n(50)
used_car_samp2 <- used_car_data %>% sample_n(50)

# Plot the sample
ggplot(used_car_samp, aes(x = log10(Miles_Driven))) + 
  geom_density()


### T-tests ###

# 1-sample t-test
t.test(x = log10(used_car_samp$Miles_Driven), 
       mu = mean(log10(used_car_data$Miles_Driven)))

## With a p-val > 0.05, we fail to reject the null hypothesis, 
## and therefore, we can conclude that there is no significant 
## difference between the sample's mean and the population's 
## mean.

# 2-sample t-test
t.test(x = log10(used_car_samp1$Miles_Driven), 
       y = log10(used_car_samp2$Miles_Driven))

## With a p-val > 0.05, we can conclude that there is no 
## significant difference in the means of the 2 samples.

# Paired t-test
t.test(x = mpg99$hwy, y = mpg08$hwy, paired = T)

## Again, with a p-val > 0.05, we can conclude that the  
## difference between the 2 sample means is equal to 0.


### Analysis of Variance (ANOVA) ###

## Is there any statistical difference in the horsepower of 
## a vehicle based on its engine type?
  ## Dependent measured variable: horsepower
  ## Independent categorical variable: number of cylinders

# Select columns for ANOVA
car_anova <- car_data[, c('hp', 'cyl', 'vs')]
car_anova$cyl <- factor(car_anova$cyl) # independent var is numeric
car_anova$vs <- factor(car_anova$vs) # so convert it to a factor (categorical)

# 1-way ANOVA
summary(aov(hp ~ cyl, data = car_anova))

## With a p-val < 0.05 on the `cyl` variable, we reject the 
## null hypothesis and accept that there is a significant 
## difference in horsepower between at least one engine type 
## and the others.

# 2-way ANOVA
summary(aov(hp ~ cyl + vs, data = car_anova))

## cyl: p < 0.05 => reject H0
## vs: p > 0.05 => fail to reject H0
## 
## We can conclude that there is a significant difference 
## in `hp` between at least one `cyl` group and the others, 
## but no difference in `hp` across all `vs` groups


### Correlation ###

# Plot horsepower against quarter-mile race time
ggplot(car_data, aes(x = hp, y = qsec)) + geom_point() + 
  labs(title = 'Horsepower vs. Quarter-mile Race Time', 
       x = 'Horsepower', y = 'Quarter-mile Race Time (sec)')

# Pearson correlation
print(cor(car_data$hp, car_data$qsec))

## Both the scatter plot and the correlation coefficient 
## show that there is a strong negative correlation 
## between `hp` and `qsec`.

# Select numeric columns and convert data to matrix
car_mat <- as.matrix(car_data %>% subset(select = disp:qsec))

# Correlation matrix
print(cor(car_mat))


### Linear Regression ###

# Simple linear regression
lr <- lm(qsec ~ hp, data = car_data) # linear model
summary(lr) # model summary

## The resulting line is: qsec = -0.02hp + 20.56
## 
## With p < 0.05, we reject the null hypothesis and conclude 
## that the slope of the line is not 0. An r-squared value 
## of 0.50 means that roughly half of this model's 
## predictions will be correct.

# Get the line equation
y <- lr$coefficients['hp'] * car_data$hp + 
     lr$coefficients['(Intercept)'] # y = mx + b

# Plot the line over the scatter plot
ggplot(car_data, aes(x = hp, y = qsec)) + 
  geom_point() + geom_line(aes(y = y), color = 'red') + 
  labs(title = 'Horsepower vs. Quarter-mile Race Time', 
       x = 'Horsepower', y = 'Quarter-mile Race Time (sec)')
  
# Multiple linear regression
mlr <- lm(qsec ~ disp + hp + drat + wt, data = car_data)
summary(mlr)

## Line: qsec = -0.01disp - 0.02hp - 0.39drat + 1.41wt + 
##              19.71
## 
## By adding in 3 more independent variables, we still 
## reject the null, but habe improved the r-squared value 
## to 0.69 (an improvement of 0.19 from simple linear 
## regression model).
## 
## The coefficients for `hp`, `wt`, and the intercept have 
## p < 0.05. So we reject the null hypothesis that these 
## coefficients contribute a random amount of variance to 
## the model. In other words, `hp`, `wt`, and the intercept 
## are statistically significant in predicting `qsec`. The 
## other 2 variables (`disp` and `drat`) have no 
## significance in this model.
## 
## Although the multiple linear regression model is far 
## better at predicting our current dataset, the lack of 
## significant variables is evidence of overfitting.


