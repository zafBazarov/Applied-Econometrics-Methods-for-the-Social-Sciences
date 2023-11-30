
## Regression Basics I

library(tidyverse)

wage_dta <- read_csv("wage-small.csv")

View(wage_dta)

glimpse(wage_dta) 

str(wage_dta)

print(wage_dta)

y <- wage_dta[,1] %>% as.matrix()

x <- wage_dta[, c(2,3)] %>% as.matrix()


#### Estimated regression coefficients
t(x) %*% x 

solve(t(x) %*% x)

solve(solve(t(x) %*% x))

t(x) %*% y

betas <- solve(t(x) %*% x) %*% t(x) %*% y


# A regression model

fit1 <- lm(formula = wage ~ educ, data = wage_dta )
fit1

### Fitted values

x %*% betas

fitted(fit1)

### Error terms

errorterms <- y - x %*% betas

errorterms

# or using `resid()` or `residuals()

residuals(fit1)


### Estimating standard errors

sigma <- t(errorterms) %*% errorterms / (nrow(y)-2)
sigma 

variance_cov <- solve(t(x) %*% x) * as.numeric(sigma)


# or

vcov(fit1)

# Standard Errors

diag(variance_cov)

sqrt(diag(variance_cov))

summary(fit1)

# Amending data with fitted values and residuals

wage_small_extended <-
  wage_dta %>%
  mutate (fitted=fitted(fit1),
          error_terms= wage - fitted)

# Residuals vs fitted plot

wage_small_extended %>%
  ggplot() + aes(x=fitted, y=error_terms) +
  geom_point()


################################################
#### Homework

#Estimate coefficients of a multiple regression and standard errors using matrices.
#Estimate following regression model:

# \text{wage} = \beta_0 + \beta_1 \text{education} + \beta_2{exper}+\beta_3{white}

x1 <- wage_dta[, c(2,3,4,6)] %>% as.matrix()
betas1 <- solve(t(x1) %*% x1) %*% t(x1) %*% y

y1 <- wage_dta[,1] %>% as.matrix()


# Add regressor `black` to the model above and estimate it again using matrices and `lm()`.

x2 <- wage_dta [, c(2,3,5, 4,6)] %>% as.matrix()

betas2 <- solve(t(x2) %*% x2) %*% t(x2) %*% y
# This does not work because of multicolleniarity. We can see that intercept is 
# equal to 1 and white is the same. 

# The solution is to remove the intercept.

x3 <- wage_dta [, c(3,5, 4,6)] %>% as.matrix()
betas3 <- solve(t(x3) %*% x3) %*% t(x3) %*% y
