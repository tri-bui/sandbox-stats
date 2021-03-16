### Dependencies and Data ###

# Libraries
# install.packages('tidyverse')
library(tidyverse)

# Built-in cars data
car_data <- mtcars


### Tests for Normality ###

# Qualitative test: density plot
ggplot(car_data, aes(x = wt)) + geom_density()

# Qualitative test: Shapiro-Wilk test
shapiro.test(car_data$wt)

## Since the p-value > 0.05, the wt variable is considered 
## normally distributed.