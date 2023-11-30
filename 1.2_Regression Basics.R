agF ## 1.2. Regression Basics

#Load (install) packages below and load the data. Packages are: 
#-   `tidyverse`
#-   `GGally`
#-   `modelsummary`
#-   `correlation`
#-   `performance`

# Data import

wage_full <- read_csv("wage-full.csv")

## 1. Glimpse at the data

glimpse(wage_full)


#Compute a new variables `caucasian` that takes values "yes" and "no" for each person depending on the values of the variable `black`.

wage_dta <-
  wage_full %>%
  mutate(caucasian = ifelse(as.logical (black), "no", "yes"),
         caucasian = as.factor(caucasian))

glimpse(wage_dta)

View(wage_dta)

### Descriptive statistics

library("modelsummary")

datasummary_skim(wage_dta)


### Visual data inspection

library(GGally)

ggpairs(wage_dta)
