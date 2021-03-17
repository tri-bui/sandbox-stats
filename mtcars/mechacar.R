### Dependencies and Data ###

# Libraries
# install.packages('pastecs')
# install.packages('tidyverse')
library(pastecs)
library(tidyverse)

# Built-in mpg data for 2008 compact cars
comp08_df <- mpg %>% filter(class == 'compact' & year == 2008)

# Suspension coil data
susp_df <- read.csv(file = 'data/Suspension_Coil.csv', 
                    stringsAsFactors = F)

# Mecha Car MPG data
mecha_df <- read.csv(file = 'data/MechaCar_mpg.csv', 
                     stringsAsFactors = F)

# Mecha Car sample with the same size as `comp08_df`
mecha_samp <- mecha_df %>% sample_n(size = nrow(comp08_df))


### Suspension Coil Analysis ###

## The suspension coils have a population mean of 1500 pounds 
## per square inch (PSI). Is there a statistical difference 
## between the sample mean and the population mean?

# Summary statistics
susp_stats <- susp_df %>% summarize(median_psi = median(PSI), 
                                    mean_psi = mean(PSI), 
                                    var_psi = var(PSI), 
                                    sd_psi = sd(PSI))

# 1-sample t-test
t.test(x = susp_df$PSI, mu = 1500)

## With a p-value of 0.5117, we fail to reject the null 
## hypothesis that the sample mean is not significantly 
## different from the population mean of 1500.

## Design specifications dictate that suspension coil 
## variance must not exceed 100 PSI. Does every manufacturing 
## lot meet this specification?

# Summary statistics for each lot
susp_stats <- susp_df %>% group_by(Manufacturing_Lot) %>% 
                          summarize(median_psi = median(PSI), 
                                    mean_psi = mean(PSI), 
                                    var_psi = var(PSI), 
                                    sd_psi = sd(PSI))

## Lots 1 and 2 have a variance of 1.15 and 10.1, 
## respectively, so they are fine. Lot 3, however, has a PSI
## variance of 220 so it does not meet this specification.


### Predicting MPG ###

# Multiple linear regression
lr <- lm(mpg ~ vehicle.length + vehicle.weight + 
               spoiler.angle + ground.clearance + AWD, 
         data = mecha_df)
summary(lr)

## With a p-value of 5.35*10^-11, we reject the null 
## hypothesis that this model's slope is equal to 0. The 
## model predicts MPG with a coefficient of determination 
## (R-squared) of 0.7149.
## 
## The intercept, `vehicle.length`, and `ground.clearance` 
## contributed a non-random amount of variance to the car 
## MPG. The other 3 variables (`vehicle.weight`, 
## `spoiler.angle`, and `AWD`) were not statistically 
## significant in this model.


### Hypothesis testing ###

# Question: Is the Mecha Car more fuel-efficient than the 2008 compact cars in the built-in 
#           MPG data?

# Abbreviations:
  # FE_m = Mecha Car's fuel efficiency
  # FE_c = 2008 Compact cars' fuel efficiency

# Hypotheses
  # Null (H0):          FE_m <= FE_c
  # Alternative (Ha):   FE_m > FE_c

# Test: 1-tailed 2-sample t-test (comparing 2 small samples)
  # The test will compare the mean MPG of 22 Mecha Cars and 22 2008 compact cars
  # Metric: mean MPG
  # Significance level: 0.05

# Summary statistics for both
stat.desc(samp_df$mpg) # Mecha Car's MPGs
stat.desc(comp_df$hwy) # compact car's MPGs

# Plot distribution of both
ggplot(samp_df, aes(x = mpg)) + geom_density() # Mecha Car's MPGs
ggplot(comp_df, aes(x = hwy)) + geom_density() # compact cars' MPGs

# 1-tailed 2-sample t-test
t.test(x = samp_df$mpg, y = comp_df$hwy, alternative = 'greater')

# The mean MPG of the Mecha Car sample was found to be 43.46, while the mean MPG of the 
# 2008 compact cars was found to be 28.73. With a p-value of 3.15*10^-5 (which is much lower 
# than our significance level of 0.05), we reject the null hypothesis. This test concludes 
# that the Mecha Car is more fuel-efficient than 2008 compact cars.

