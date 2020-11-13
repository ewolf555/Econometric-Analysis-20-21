library(AER)
library(readxl)
library(car)

# Import data
ex_12 <- read_excel("...")

#IV Estimation
model_ivreg <- ivreg(INFLAT ~ MONEY + OUTPUT | MONEY + INV + POPRATE + SCHOOL + INITIAL, data = ex_12)

summary(model_ivreg, diagnostics = T)

# F-Test for the joint hypothesis
linearHypothesis(model_ivreg, c("(Intercept) =0", "MONEY = 1", "OUTPUT = -1"), test = "F")
