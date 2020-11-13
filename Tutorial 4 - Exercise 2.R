library(readxl)
library(lmtest)
library(nlme)
library(sandwich)

setwd("...")
data_phillips <- read.csv("phillips.csv")

# Estimate Model with OLS
model_ols <- lm(inf ~ du, data = data_phillips)
summary(model_ols)

# Test for Autocorrelation
plot(model_ols$residuals, type="l")
acf(model_ols$residuals)
bgtest(inf ~ du, order = 8, data = data_phillips)

# Estimate Model with GLS

# Estimate the Corrleation Structure of Residuals 
ar_res <- ar(model_ols$residuals, aic = TRUE)
print(ar_res)

# Estimate Model 
model_gls <- gls(inf ~ du, correlation = corARMA(ar_res$ar, p=3), data = data_phillips)
summary(model_gls)

# Estimate and compute the HAC Standard Errors
hac_errors <- vcovHAC(model_ols)

coeftest(model_ols, vcov. = hac_errors)

