##########################
# Webinar 1 Econometrics #
##########################

# Linear models in R

# Example 1 Return to schooling
library(tidyverse) # манипуляции с данными
library(skimr) # описательные статистики
library(AER) # пакет с данными 

# Set your working directory
setwd(" ")

data("CPS1988")
help("CPS1988")
head(CPS1988)
glimpse(CPS1988)
str(CPS1988)

qplot(data = CPS1988, education, wage)
qplot(data = CPS1988, education, wage) +
  labs(x = 'Образование',
       y = 'Заработная плата',
       title = 'Данные по з/п и образованию',
       subtitle = '(встроенный в R набор CPS1988)')

mydata <- with(data = CPS1988,
               subset(x = CPS1988, subset = experience > 1 & education > 1))
glimpse(mydata)

qplot(mydata$education, binwidth = 0.7)
qplot(mydata$wage, binwidth=20)
plot(mydata$education, mydata$wage, main="Scatterplot", 
     xlab="education", ylab="wage", pch=19)

# Описательные статистики
summary(mydata)
skim(mydata)

model <- lm(data = mydata, wage ~ education)
summary(model)
coef(model)

y = mydata$wage
TSS = sum((y - mean(y))^2)
TSS

RSS = deviance(model)
RSS

ESS = TSS - RSS
ESS

R2 = ESS / TSS
R2

nd <- data.frame(education=c(10,3))
predict(model, newdata = nd)

modelmult1 <- lm(data = mydata, wage ~ education +experience)
summary(modelmult1)

modelmult2 <- lm(data = mydata, wage ~ education + experience + I(experience^2))
summary(modelmult2)

nd <- data.frame(education= c(10,3), experience = c(20,8))
predict(modelmult2, newdata = nd)

# Example 2 Return to schooling - Data from stata 
library(foreign)
data <- read.dta("Dougherty.dta")

library("readxl")
data  <- read_excel("Dougherty.xlsx",sheet = "Sheet1") 

#regression estimation
model_long <- lm(data=data, S~ASVABC)
model_short <- lm(data=data, S~ASVABC+SM+SF)
