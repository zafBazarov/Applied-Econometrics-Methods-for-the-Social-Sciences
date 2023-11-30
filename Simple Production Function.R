################################
### Simple Production Function
#########################################

# 1. Libraries 

library(tidyverse)
library(GGally)
library(modelsummary)
library(performance)
library(lmtest)

# 2. Import data
farms <- read_csv("rice.csv")
farm_dta <- farms %>% select(output, land, labor, status)
# we select some data from farms.

glimpse(farms)

glimpse(farm_dta)

# 3. Exploratory data analysis

farm_dta %>% datasummary_skim()


# 4. Estimating regression

fit2_cd <- lm(output ~ 1, data = farm_dta)
fit2_cd <- lm (output ~ land + labor+ status, data = farm_dta)

# fit2_cde1 <- lm (log (output) ~ log (land) + log (labor)+ status,data = farm_dta )
# there are missing values (NA), infinite values (Inf), or non-numeric values (NaN) in the input data 
# used for the linear regression model fitting.

fit2_cde2 <- lm (log (output) ~ log (land) + log (labor)+ status,data = farm_dta %>% filter(land>0) )


# 5. Validating assumptions
farm_dta %>% filter(output==0)
farm_dta %>% filter(labor==0)
farm_dta %>% filter(land==0)
farm_dta %>% filter(land>0) 
# this solves our problem because we could not make log log because of 0 values in land.


# 6. Reporting regression results

summary(fit2_cde) # it  is a generic function used to produce result summaries of the results of various model fitting functions.

modelsummary(fit2_cde) # Create beautiful and customizable tables to summarize several statistical models side-by-side.


# 7. New regression variant

farm_aug <- farm_dta %>% 
  # filter(land==0) %>% 
  mutate(land=ifelse(land<=0, 0.0001, land))

fit3_cd <- lm (log (output) ~ log (land) + log (labor)+ status,data = farm_aug)

summary(fit3_cd)

modelsummary(fit3_cd)

modelsummary(
  list("Cool name of the mode" =
         fit2_cde,fit3_cd),
  estimate = '{estimate} ({std.error}){stars}',
  statistic = NULL
)

# 8. Robust standard error

modelsummary(
  list(`Log-Log-1`= fit2_cde, `Log-Log-2`=fit3_cd)         ,
  estimate = '{estimate} ({std.error}){stars}',
  statistic = NULL,
  vcov = "HC3")  # we need sandwich package for that

# 9. Check function

check_model(fit2_cde,
            check = c( "linearity", "homogeneity") ) # this model is looks like linear

check_model(fit3_cd,
            check = c( "linearity", "homogeneity") )

# 10. Breusch-Pagan test

bptest(fit2_cde) # detects heteroskedasticity 

bptest(fit3_cd) # has heteroskedasticity

# 11. Check for Multicollinearity

check_collinearity(fit2_cde) # Low correlation


