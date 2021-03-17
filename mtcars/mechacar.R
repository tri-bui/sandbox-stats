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


### Fuel Economy Prediction ###

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
## 
## This means that the length of the vehicle and its ground 
## clearance has a significant impact on its fuel economy. 
## The significance of the intercept indicates that there are 
## other factors affecting the fuel economy that were not 
## used in the model. This would require additional features.


### Hypothesis Testing ###

## Question: Is the Mecha Car more fuel-efficient than the 
##           2008 compact cars in the built-in MPG data?
## 
## Abbreviations:
  ## mpg_m = Mecha Car's fuel efficiency (MPG)
  ## mpg_c = 2008 Compact cars' fuel efficiency (MPG)
## 
## Hypotheses:
  ## Null (H0):          mpg_m <= mpg_c
  ## Alternative (Ha):   mpg_m > mpg_c
## 
## Test: 1-tailed 2-sample t-test
  ## The test will compare the mean MPG of 22 randomly 
  ## sampled Mecha Cars and 22 2008 compact cars
  ## Metric: mean MPG
  ## Significance level: 0.05

# Summary statistics for both
mecha_stats <- stat.desc(mecha_samp$mpg) # Mecha Cars' mpg
comp08_stats <- stat.desc(comp08_df$hwy) # compact cars' mpg

# Plot distribution of both
ggplot(mecha_samp, aes(x = mpg)) + geom_density()
ggplot(comp08_df, aes(x = hwy)) + geom_density()

# 1-tailed 2-sample t-test
t.test(x = mecha_samp$mpg, y = comp08_df$hwy, 
       alternative = 'greater')

## The mean fuel economy of the Mecha Car sample and the 2008 
## compact cars were found to be 44.40 and 28.73, 
## respectively. With a p-value of 1.55*10^-5 (much lower 
## than our significance level of 0.05), we reject the null 
## hypothesis. This test concludes that the Mecha Car is 
## statistically more fuel-efficient than 2008 compact cars.