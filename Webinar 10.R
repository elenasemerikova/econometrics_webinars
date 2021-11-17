###  Heteroskedasticity  ###

# @ Elena Semerikova 2021 

# Packages ----------------------------------------------------------------
library(tidyverse)
library(lmtest)
library(AER)
library(car)
library(MASS)
library(ggplot2)

# Data Import and Review --------------------------------------------------
data("CPS1988") # Load 1988 Census data from AER package
help("CPS1988") # Get info

# Subsetting and Managing Data --------------------------------------------
data_cps <- with(data = CPS1988,
                 subset(x = CPS1988, subset = experience > 1 & education > 1))
summary(data_cps)

# Heteroskedasticity ------------------------------------------------------
model1 <- lm(formula = wage ~ education, data = data_cps)
summary(model1)
scatter.smooth(x=data_cps$education,  y=data_cps$wage, main="wage ~ education")
scatter.smooth(x=data_cps$education,  y=log(data_cps$wage), main="wage ~ education")

## Heteroskedasticity tests
## Goldfeld-Quandt
gqtest(model1, order.by = data_cps$education, fraction = 0.25)

## White test
model1 <- lm(formula = wage ~ education, data = data_cps)
data_cps$resid <- residuals(model1)   

model2 <- lm(formula = wage ~ education + experience, data = data_cps)
bptest(formula = model2, varformula = ~ education + I(education^2) + experience + I(experience^2) + education*experience , data = data_cps)

whitemodel = lm(formula = I(resid^2) ~ education + I(education^2), data = data_cps)
summary(whitemodel)
Chi = 0.0006255*26097
Chi

## Correcting for heteroskedasticity
coeftest(model1)
vcov(model1)

vcovHC(model1)
model1 <- lm(formula = wage ~ education, data = data_cps)
coeftest(model1, vcov. = vcovHC(model1))
summary(model1, vcov. = vcovHC(model1))

