
## Regression Basics II

# libraries
library(tidyverse)
library(readr)

# Import the data

wage_full <- read_csv("wage-full.csv")

## Glimpse at the data

glimpse(wage_full)

# Compute a new variables `caucasian` that takes values "yes" and "no" for each 
# person depending on the values of the variable `black`.

wage_dta <-
  wage_full %>%
  mutate(caucasian = ifelse(as.logical (black), "no", "yes"),
         caucasian = as.factor(caucasian))

glimpse(wage_dta)

#Descriptive statistics
# I use datasummary_skim() function from the package modelsummary.
library(modelsummary)

datasummary_skim(wage_dta)


# Visual data inspection
# we use data visually by building a grid of scatter plots using 'GGally': ggpairs()
library(GGally)
ggpairs(wage_dta)

# or we can just reduce some variable to show more information.

wage_dta1 <- wage_dta %>% select(-black, -white)
  ggpairs(wage_dta1)

# we add parameter `aes(colour = caucasian)` to `ggpairs()` to make our plot more colorfull.
  
wage_dta %>% select(-black, -white) %>% ggpairs(aes(colour = caucasian))

## Fit regressions
#Dependent variable is `wage`, independent are: `educ`, `exper` and `black`

fit2 <- lm(wage ~ educ+exper+black, data = wage_dta)
fit2

# Regression summary
summary(fit2)


# Regression summary using 'parameters:: parameters()
library(parameters)
parameters(fit2)

# Goodness of fit using 'performance:: performance()

library(performance)
performance(fit2)

# Fitted value
fitted_vector <- fitted(fit2)
fitted_vector[1:20]

# Residuals

resid_vector <- resid(fit2)
resid_vector[1:20]

# Residuals vs fitted plot
# I use plot() function. Put fitted as an 'x' argument and residuals as 'y'
plot(x=fitted_vector, y=resid_vector)

# Residuals vs fitted using 'check_model' from 'perforamnce' package
library(patchwork)
check_model(fit2, check = "linearity")
check_model(fit2)

# Predicting values 

#We can compute predicted values based on the regression results for arbitrary values of $X$. See: `?predict`
#First, we need to make a table the $X$ variables for which we want compute the predicted value.
# Variables names in table should match the variables in the regression.
#Let us predict wage for a person with 0 years of education, 0 experience, when he/she is white.

pred1 <-
  tibble(educ=0,
         exper = 0,
         black =0)

predict(fit2, pred1)

# The same when the person is black

pred2 <-
  tibble(educ=c(0,0),
         exper = c(0,0),
         black =c(0,1))

predict(fit2, pred2)


########### EX1.Writing predicted values to a data frame

# use mutate to mutate new variable in a data frame: 

pred3 <- 
  tibble(educ = c(0,0,10),
         exper = c(0,0,25),
         black = c(0,1,1)
         )

pred3 %>%
  mutate(predicted = predict(fit2, pred3))

### EX2. Visual inspection of predicted values

# we can use function 'ggeffects::ggpredict()', check help.
library("ggeffects")
ggpredict(fit2, term="educ")

# we can also plot this effects using 'plot' after 'ggpredict'

ggpredict(fit2, term="educ") %>% plot()


# Compute predicted values for a different independent variables and a dummy variable.
fit3 <- lm (wage ~ educ + exper + black + female, data=wage_dta)
ggpredict(fit3, term= c("educ", "female"))

# plot
ggpredict(fit3, term = c("educ", "female")) %>% plot()

# we can do this plot for experience

ggpredict(fit3, term = c("educ", "exper")) %>% plot()





