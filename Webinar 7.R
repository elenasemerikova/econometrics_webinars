## Webinar 7

library(tidyverse)
library(lmtest)
library(AER) # data are also there
library(skimr) 

# Data Import -------------------------------------------------------------
#setwd("")
data_credit <- read.csv("CreditCard.csv")
str(data_credit)
help("CreditCard")
summary(data_credit)
glimpse(data_credit)
skim(data_credit)


# Empirical Distribution --------------------------------------------------
## Suspicious values of age < 1. We shall cut them off
hist(data_credit$age)
hist(data_credit$income, breaks = 30)
hist(log(data_credit$income), breaks = 30)

hist(data_credit$expenditure, breaks = 30)
hist(subset(data_credit$expenditure, data_credit$expenditure > 0), breaks = 30)
hist(subset(log(data_credit$expenditure), data_credit$expenditure > 0), breaks = 30)

## Filtered dataset
data2 <- with(data = data_credit,
              subset(x = data_credit, subset = income > 1.5 & expenditure > 0 & age > 1))
data2$log_inc <- log(data2$income)
data2$log_expen <- log(data2$expenditure)

## Cross-correlation plots
plot(data2[, c("age", "income", "expenditure")])
plot(data2[, c("age", "log_inc", "log_expen")])

# OLS ---------------------------------------------------------------------
## First simple hypothesis: higher income means higher expenditures
lm1 <- lm(expenditure ~ income, data = data2)
summary(lm1)
## Additional 10000 USD of yearly income increases average monthly expenditures by 50

## Log-log model
lm2 <- lm(log(expenditure) ~ log(income), data = data2)
summary(lm2)
## Increase in yearly income by 1% increases average monthly spendings by 0.62% on average

## Second: income is quadratic relative to age: income = beta_0 + beta_1*age + beta_2*age^2
lm3 <- lm(log(income) ~ age + I(age^2), data = data2)
summary(lm3)
## Both beta_1 and beta_2 are significantly different from zero
with(data = data2, plot(x = age, y = log(income)))


# Dummy
data_credit$dummy = ifelse(data_credit$card == "yes", 1, 0)
lm4 = lm(expenditure ~ income + age + dummy, data = data_credit)
summary(lm4)
resettest(lm4)

lm5 = lm(expenditure ~ income + age +  dummy*income, data = data_credit)
summary(lm5)
lm6 = lm(expenditure ~ income + age, data = data_credit)
summary(lm6)

waldtest(lm5, lm6)

# Additional fancy plots
ggplot(data = data_credit %>% pivot_longer(cols = c(age, income, expenditure), names_to = "variable"),
       aes(x = value, group = variable)) +
  geom_histogram() +
  facet_wrap(facets = vars(variable), scales = "free")

with(data = data2,
     plot(x = log(income), y = log(expenditure)))
abline(a = lm2$coefficients[1], b = lm2$coefficients[2], col = "red", lwd = 1.5)

curve(expr = lm3$coefficients[1] + lm3$coefficients[2] * x + lm3$coefficients[3] * x^2,
      from = 20, to = 80, n = 100,
      col = "red", lwd = 1.5, add = TRUE)
