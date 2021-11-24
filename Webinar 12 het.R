### Autocorrelation Screencast ###

# Packages ----------------------------------------------------------------
library(car)

# Import Data -------------------------------------------------------------
data(freeny)
help(freeny)

# Models ------------------------------------------------------------------
model1 <- lm(data = freeny, y ~ price.index + income.level + market.potential)
summary(model1)

model2 <- lm(data = freeny, y ~ price.index )
summary(model2)

# Plotting Residuals ------------------------------------------------------
res_df = data.frame(time = as.numeric(time(freeny$y)),
                     model1 = as.numeric(residuals(model1)),
                     model2 = as.numeric(residuals(model2)))

## Residuals time plot
plot(res_df$model1)
plot(res_df$model2)

n <- nrow(freeny)

lag_df <- data.frame(time = as.numeric(time(freeny$y)),
                     model1 = c(NA, res_df$model1[1:(n-1)]),
                     model2 = c(NA, res_df$model2[1:(n-1)]))

## Residuals vs. lagged residuals
plot(res_df$model1, lag_df$model1)
plot(res_df$model2, lag_df$model2)
# model 2 autocorrelation due to omitted parket potential

## Histogram
hist(res_df$model1)
hist(res_df$model2)


# Autocorrelation Testing -------------------------------------------------
## Durbin-Watson Test

dwt(model1,  alternative = "two.sided")
dwt(model1,  alternative = "positive")
dwt(model1,  alternative = "negative")

dwt(model2,  alternative = "positive")
dwt(model2,  alternative = "two.sided")
# positive autocorrelation for model 2 due to autocorrelated omitted vairable


## Breusch-Godfrey Test
bgtest(model1, order = 1)
bgtest(model2, order = 1)

bgtest(model1, order = 2)
bgtest(model2, order = 2)

# Autocorrelation Correction ----------------------------------------------
vcov(model2)
vcovHAC(model2)
coeftest(model2, vcov = vcovHAC(model2))
