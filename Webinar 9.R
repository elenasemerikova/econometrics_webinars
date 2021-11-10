##########################
# Webinar 9 Econometrics #
##########################

# Linear models in R
install.packages("tidyverse")

# Example 1 Return to schooling
library(tidyverse) # манипуляции с данными
library(skimr) # описательные статистики
library(AER) # пакет с данными 
library(quantreg) # пакет с данными
library(ggplot2)
library(glmnet)
library(car)
library(corrplot)
library(glmnet)  # LASSO + ridge
library(factoextra)

# Set your working directory
setwd("~/Documents/2021_2022/Эконометрика Вебинары/Вебинар 9")

# Example 2 Return to schooling - Data from stata 
library(foreign)
data <- read.dta("Dougherty.dta")

#Multicollinearity
model1 <- lm(data=data, EARNINGS ~ ASVABC + S + SM + SF)
summary(model1)
vif(model1)
datasmall = tibble(data$EARNINGS, data$ASVABC, data$S, data$SM, data$SF)
cor(datasmall)
corrplot.mixed(cor(datasmall))

# LASSO
lambda = seq(from = 50, to = 0.01, length = 30)
lambda
y = data$EARNINGS
X0 = model.matrix(data = data, EARNINGS ~ 0 + ASVABC + S + SM + SF)

y
X0

m_lasso = glmnet(X0, y, alpha = 1, lambda = lambda)
m_lasso
# %Dev - процент объясненного разброса R2

m_ols = lm(data = data, EARNINGS ~ ASVABC + S + SM + SF)
summary(m_ols)

plot(m_lasso, xvar = 'lambda', label = TRUE)
plot(m_lasso, xvar = 'dev', label = TRUE)
plot(m_lasso, xvar = 'norm', label = TRUE)
coef(m_lasso, s = c(1.73, 10.35), exact = TRUE, x = X0, y = y)

# Ridge
m_ridge = glmnet(X0, y, alpha = 0, lambda = lambda)
m_ridge
coef(m_ridge, s = c(1.73, 10.35), exact = TRUE, x = X0, y = y)
plot(m_ridge, xvar = 'lambda', label = TRUE)
plot(m_ridge, xvar = 'dev', label = TRUE)
plot(m_ridge, xvar = 'norm', label = TRUE)

cv_lambda = cv.glmnet(X0, y, alpha = 1)
plot(cv_lambda)

cv_lambda$lambda.min
cv_lambda$lambda.1se
coef(cv_lambda, s = 'lambda.1se')
coef(cv_lambda, s = 'lambda.min')

# PCA
datatests = tibble(data$ASVAB01, data$ASVAB02, data$ASVAB03, data$ASVAB04, data$ASVAB05, data$ASVAB06)
d_pca = prcomp(datatests, scale = TRUE)
pca1 = d_pca$x[, 1]
head(pca1)


v1 = d_pca$rotation[, 1]
v1
summary(d_pca)
cor(data$ASVABC, pca1)
cor(data$S, pca1)
fviz_eig(d_pca)
fviz_pca_biplot(d_pca, repel = TRUE)
