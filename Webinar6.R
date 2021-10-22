qt(0.975, df = 1000)

#install_formats()
library(rio)
data  <- import("data_webinar1.csv") 
summary(data)
model1 <- lm(formula = sales ~ equipment_spendings + n_employees, data = data)
summary(model1) 
confint(model1, level = 0.90)
# Make prediction
nd <- data.frame(equipment_spendings = c(800000, 100000), n_employees = c(26, 36))
predict(model1, newdata = nd)
