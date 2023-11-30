
############################
## Regression analysis
#######################################

# 1. Libraries 

library(tidyverse)
library(GGally)
library(modelsummary)
library(performance)
library(lmtest)
library(readr)

# Import Data set

rice <- read_csv("rice.csv")

glimpse(rice)

# Explonatory data analysis

rice %>% datasummary_skim(title = "Descriptive statistics of the numeric vaiables")

rice %>% datasummary_skim( type="categorical", title = "Descriptive statistics of the categorical vaiables")


# Estimation Regression

names(rice)
m1 <- lm(output ~ status + land  +labor + labor_family +labor_hired
+  seed + urea + phosphate + pesticide + region, data = rice )

summary(m1)

m2 <- lm(log(output) ~ status + log (land)  + log (labor)  + log (labor_family) 
         + log (labor_hired) + log  (seed) + log(urea) + log (phosphate) + log (pesticide) + region, data = rice )
# fit2_cde1 <- lm (log (output) ~ log (land) + log (labor)+ status,data = farm_dta )
# there are missing values (NA), infinite values (Inf), or non-numeric values (NaN) in the input data 
# used for the linear regression model fitting.
 
# We need to filter the our data

rice %>% filter(output<=0) # no 0 values
rice %>% filter(land<=0)   # it has
rice %>% filter(labor<=0)  # no 
rice %>% filter(labor_family<=0)  # no
rice %>% filter(labor_hired<=0)  # no
rice %>% filter(seed<=0)  # no
rice %>% filter(urea<=0)  # no
rice %>% filter(phosphate<=0)  # yes
rice %>% filter(pesticide<=0)  # yes

# we run regression now
m3 <- lm(log(output) ~ status + log (land)  + log (labor)  + log (labor_family) 
         + log (labor_hired) + log  (seed) + log(urea) + log (phosphate) + log (pesticide) + region, 
         data = rice %>% filter(land>0, phosphate>0, pesticide>0 ) )

summary(m3) # we have only around 300 observations degree of freedom. 

# We need a solution to make our data
# we create a new data

rice2 <- 
  rice %>% 
  mutate (land = ifelse(land<=0, 0.0001, land), 
          phosphate = ifelse(phosphate<=0, 0.0001, phosphate),
          pesticide = ifelse(pesticide<=0, 0.0001, pesticide)
          )

# we run regression now
m4 <- lm(log(output) ~ status + log (land)  + log (labor)  + log (labor_family) 
         + log (labor_hired) + log  (seed) + log(urea) + log (phosphate) + log (pesticide) + region, 
         data = rice2 )

summary(m4)

# to compare regressions we do next

modelsummary(
  list(
    `Filtered data regression` = m3, 
    `Log()=log(0.0001)` = m4), 
  statistic = NULL, 
  estimate = "{estimate}{stars} ({std.error}"
   ) #vcov = "HC3" we need sandwich package for that


# Observation is bigger m4 here. that is good. 
# also the land is important variable for us. it is changed dramatically in m4.

# to check our data or se we do next model
modelsummary(
  list(
    `Filtered data regression` = m3, 
    `Log()=log(0.0001)` = m4,
  `Log()=log(0.0001) (cl. s.e.)` = m4),
  statistic = NULL, 
  estimate = "{estimate}{stars} ({std.error})",
  vcov = list (vcov, vcov, ~region)    # covariance 
  )

#  we change our data again
rice3 <- 
  rice %>% 
  mutate (land_reverse = ifelse(land<=0, 1, 0),
          land = ifelse(land<=0, 1, land),
          phosphate_reverse = ifelse(phosphate<=0, 1,0), 
          phosphate = ifelse(phosphate<=0, 1, phosphate),
          pesticide_reverse = ifelse(pesticide<=0, 1, 0),
          pesticide = ifelse(pesticide<=0, 1, pesticide)
  )
# to add reverse variable here is taken from a model for dummy variables

# we run regression now
m5 <- lm(log(output) ~ status + log (land)  + log (labor)  + log (labor_family) 
         + log (labor_hired) + log  (seed) + log(urea) + log (phosphate) + log (pesticide) + region +
           land_reverse + phosphate_reverse + pesticide_reverse, 
         data = rice3 )

# to check our data or se we do next model
modelsummary(
  list(
    `Filtered data regression` = m3, 
    `Log()=log(0.0001)` = m4,
    `Log()=log(0.0001) (cl. s.e.)` = m4,
  `Reverse dummy` = m5),
  statistic = NULL, 
  estimate = "{estimate}{stars} ({std.error})",
  vcov = list (vcov, vcov, ~region, ~region)    # we will drop of this 
)

# Linearity

check_model(m5, check= c("linearity", "homogeneity"))

# Multicollinearity

check_collinearity(m5)

# Homoscedasticity

check_heteroscedasticity(m5)

bptest(m5)  # we had a heteroscedastic problem in our data


# Reporting regression results

modelsummary(
  list(
    `CD Production function` = m5),
  statistic = NULL, 
  estimate = "{estimate}{stars} ({std.error})",
  vcov = list ( ~region) ,
  title= "CD production function regression results",
  coef_rename = c("statusowner" = "Owner operated farm", "log(land)"= "Land ha")
)
