library(AER)
library(readxl)
library(car)

# Import data
ex_12 <- read_excel("...")

# OLS Estimation 
model_ols <- lm(INFLAT ~ MONEY + OUTPUT, data = ex_12)

summary(model_ols)


# F Test for the joint hypothesis
linearHypothesis(model_ols, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), test = "F")


#IV Estimation
model_ivreg <- ivreg(INFLAT ~ MONEY + OUTPUT | MONEY + INV + POPRATE + SCHOOL + INITIAL, data = ex_12)

summary(model_ivreg, diagnostics = T)

# F-Test for the joint hypothesis
linearHypothesis(model_ivreg, c("(Intercept) =0", "MONEY = 1", "OUTPUT = -1"), test = "F")
