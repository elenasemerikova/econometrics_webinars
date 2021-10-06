##########################
# Webinar 4 Econometrics #
##########################

# Packages ----------------------------------------------------------------
library(tidyverse)
library(lmtest)
library(AER)
library(ggplot2)

# Data Import and Review --------------------------------------------------
data("CPS1988") # Load 1988 Census data from AER package
help("CPS1988") # Get info
#A sample of men aged 18 to 70 with positive annual income 

str(CPS1988) # Get variable info
head(CPS1988) # View top rows

# Subsetting and Managing Data --------------------------------------------
data_cps <- with(data = CPS1988,
                 subset(x = CPS1988, subset = experience > 1 & education > 1))
summary(data_cps)

# Part 2
# Simple Linear Regression ------------------------------------------------
model1 <- lm(formula = wage ~ education, data = data_cps)
summary(model1)

#Confidence intervals for estimates
confint(model1)

# includes zero - significant
# not - insignificant

ggplot(data = data_cps, mapping = aes(x = education, y = wage)) +
  geom_point() +
  geom_smooth(aes(x = education, y = wage), formula = y ~ x, method = "lm", colour = "red3")

# plot in logarithmic scale
ggplot(data = data_cps, mapping = aes(x = education, y = wage)) +
  geom_point() +
  geom_smooth(aes(x = education, y = wage), formula = y ~ x, method = "lm", colour = "red3") +
  scale_y_log10()

## Testing Linear Restrictions
coeftest(model1)
linearHypothesis(model1, "education = 52")
linearHypothesis(model1, "education = 50")

# Predictions
nd <- data.frame(education = c(10, 3))
predict(model1, newdata = nd)
predict(model1, newdata = nd, interval = "confidence", level=0.95)
predict(model1, newdata = nd, interval = "prediction", level=0.95)

# prediction interval predicts in what range a future individual observation will fall 
# while a confidence interval shows the likely range of values  for population conditional mean.


# Multiple Regression -----------------------------------------------------
model2 <- lm(formula = wage ~ education + experience, data = data_cps)
summary(model2)

ggplot(data = data_cps, mapping = aes(x = experience, y = wage, group = education)) +
  geom_point() +
  geom_smooth(aes(x = experience, y = wage), formula = y ~ x, method = "lm", colour = "red3") +
  facet_wrap(facets = vars(education)) +
  scale_y_log10()
# for those who have few years of education the effect of the experience is lower

## Relationship is most certainly quadratic, so consider non-linear experience term
model3 <- lm(formula = wage ~ education +  experience + I(experience^2), data = data_cps)
summary(model3)

scatter.smooth(x=data_cps$experience,  y=log(data_cps$wage), main="wage ~ education +  experience + I(experience^2)", col = "grey" )

ggplot(data = data_cps, mapping = aes(x = experience, y = wage, group = education)) +
  geom_point() +
  geom_smooth(aes(x = experience, y = wage), formula = y ~ x + I(x^2), method = "lm", colour = "red3") +
  facet_wrap(facets = vars(education)) +
  scale_y_log10() 
## Much better

# Linear hypothesis
linearHypothesis(model3, "education + experience = 90")
linearHypothesis(model3, "education = 1.9 * experience")
linearHypothesis(model3, "I(experience^2) = 0") # Squared term is indeed important

# Make prediction
nd <- data.frame(education = c(10, 3, 5), experience = c(10, 20, 30))
predict(model3, newdata = nd)
predict(model3, newdata = nd, interval = "confidence")
predict(model3, newdata = nd, interval = "prediction")

