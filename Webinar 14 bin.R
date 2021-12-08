# Скачиваем и подключаем необходимые пакеты
#install.packages("AER")   # пакет, в котором поставляется исследуемый датасет
library("AER")
#install.packages("margins") # пакет для расчета предельных эффектов в моделях бинарного выбора
library("margins") 
library(skimr)
library(huxtable)
library(dplyr)

data("CreditCard")                # рассматриваем данные по выдаче кредитных карт 
help("CreditCard")                # смотрим подробно информацию по всем переменным
data_base <- CreditCard  # для удобства переназываем датасет

# transform the data to binary with values 0 and 1

data_base$card <- as.numeric(data_base$card)
data_base$card[data_base$card == 1] <- 0
data_base$card[data_base$card == 2] <- 1
data_base$selfemp <- as.numeric(data_base$selfemp)
data_base$selfemp[data_base$selfemp == 1] <- 0
data_base$selfemp[data_base$selfemp== 2] <- 1

data_basef = mutate_at(data_base, vars(card,selfemp), factor)
  
skim(data_basef)


# Будет моделироваться вероятность выдачи индивиду кредитной карты
# в зависимости от количества плохих отчетов о его прошлой кредитной истории (отрицательное влияние),
# от его дохода (положительное влияние)
# и от факта того, является ли он самозанятым - дамми переменная (отрицательное влияние)

####### Пробит Модель #######
model_probit <- glm(formula = card ~ reports + income + selfemp,
                    data = data_basef,
                    family = binomial(link = "probit"))
model_logit <- glm(formula = card ~ reports + income + selfemp,
                    data = data_basef,
                    family = binomial(link = "logit"))
summary(model_probit)
confint(model_probit)
summary(model_logit)
huxreg(model_probit, model_logit)
#The null deviance shows how well the response is predicted by 
# the model with nothing but an intercept.

#The residual deviance shows how well the response is predicted
#by the model when the predictors are included.

#  the deviance goes down by 320 => good fit
# AIC

# Let's calculate the marginal effect of the variable income
# for a concrete individual with the following characteristics:
# annual income = 20 000 dollars
# 1 bad credit history item

individual <- data.frame("reports" = 1,                           # укажем характеристики
                       "income" = 2,
                       "selfemp" = "0")                           # нашего индивида в датафрейме

individual_me_income <- margins(model = model_probit, at = individual,
                          variables = "income")                   # оценим предельный эффект по переменной income
                                                                 # для нашего индивида
individual_me_income # отобразим предельный эффект

# Increase of income by 1 thousand dollars increases the probability
# by 5% ceteris paribus for an individual 

library(ggplot2)
ggplot(data = data_base, aes(x = income, y = card))+
  geom_smooth(method = "glm", method.args = list(family = "binomial"(link = "logit"))) + geom_point()

new <- data.frame("reports" = 1,                           # укажем характеристики
                  "income" = 2:3,
                  "selfemp" = "1")      #внимательно к значениям факторных переменных
predict(model_probit, new, type="response")

#install.packages("broom") 
library("broom")                                           #для прогнозирования

#прогнозирование скрытой переменной 
pr <- augment(model_probit, data_basef)
#(столбец .fitted)
pr

#прогнозируем вероятность, что card = 1 (вероятность выдачи кредитной карты)
pr_prob <- augment(model_probit, data_base, type.predict = "response")
#(столбец .fitted)
pr_prob

#install.packages("AUC") 
library("AUC")                                      #для постороения ROC-кривой
library("ggplot2")

#FPR - false positive rate, TPR - true positive rate
roc.data <- roc(pr_prob$.fitted, factor(pr_prob$card))
str(roc.data)
auc(roc.data)


#чувствительность  =TPR = true positive rate = TP / (TP + FN)
qplot(x = roc.data$cutoffs, y = roc.data$tpr, geom = "line", 
      xlab = "Cutoff", ylab = "Sensitivity")

#специфичность Specificity = 1- false positive rate, fpr = FP/(FP+TN)
qplot(x = roc.data$cutoffs, y = (1-roc.data$fpr), geom = "line", 
      xlab = "Cutoff", ylab = "Specificity")

#ROC-кривая
#по горизонтали - процент ложноположительных прогнозов, false positive rate
#по вертикали - чувствительность  true positive rate Sensitivity
qplot(x = roc.data$fpr, y = roc.data$tpr, geom = "line", 
      xlab = "FPR", ylab = "TPR", main = "ROC-curve")



