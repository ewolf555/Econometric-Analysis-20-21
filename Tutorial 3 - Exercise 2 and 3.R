library(tseries)

setwd("...")
# Quick example for inflated variance

data_t_test <- read.csv("t-test_1.csv")

# estimate first model 
mod_1 <- lm(y ~ x_1, data_t_test)
summary(mod_1)

# estimate first model 
mod_2 <- lm(y ~ x_2, data_t_test)
summary(mod_2)

# add highly correlated regressor x_12
cor(data_t_test$x_1, data_t_test$x_2)

# estimate second model
mod_3 <- lm(y ~ x_1 + x_2, data_t_test)
summary(mod_3)


# Part 2
dataset <- read.csv("A1Data2018.csv")


# Part 1
# Transform Variables
dataset_log <- log(dataset)

# Estimate Model
model <- lm(Cost ~ output + Plabor + Pcapital + Pfuel, data = dataset_log)

# obtain summary statistics
res <- summary(model)

print(res)

# F-Test 
# specify restictions
R = c(0, 0, 1, 1, 1)
r = 1

# caluclate X'X matrix and inverse
coef <- model$coefficients
X <- cbind(rep(1, nrow(dataset_log)), as.matrix(dataset_log[2:5]))
XX_inv <- solve(t(X) %*% X)

# first term of F-statistic numerator
F_stat_t1 <- t(R %*% coef - r)

# second term of F-statistic numerator
F_stat_t2 <- solve(t(R) %*% XX_inv %*% R)

# calculate F-statistic
F_stat<- (F_stat_t1 %*% F_stat_t2 %*% t(F_stat_t1))/res$sigma^2

# calculate P-value
pval_f <- 1-pf(F_stat, df1 = 1, df2 = 140)   


################################################################################
################################################################################

# load data
data <- read.csv("t-test_2.csv")

# data length
n <- nrow(data)
print(n)

# estimate model
model <- lm(y ~ x + z + y_1, data = data)


# investigate normality of residuals
hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)
jarque.bera.test(model$residuals)


# nonparamteric Bootstrap Test:

# estimate model under H_0
model_H0 <- lm(y ~ x + y_1, data = data)

# store coefficients
resids <- model_H0$residuals
coeffs <- model_H0$coefficients

# simulate data with 1000 draws
B <- 1000

y_sim <- list()


for (j in 1:B){
  # sample residuals
  set.seed(j)
  u_sample <- sample(resids, n, replace = TRUE)
  
  # generate simulated data for y
  y_sample <- numeric(length = n)
  
  y_sample[1] <- coeffs[1] +  data$x[1]*coeffs[2] + data$y[1]*coeffs[3] + u_sample[1] 
  
  for (i in 2:n){
    y_sample[i] <- coeffs[1] + coeffs[2]*data$x[i] + coeffs[3]*y_sample[i-1] + u_sample[i] 
  }
  
  # store simulated data in list
  y_sim[[j]] <- y_sample
}

# estimate model based on simulated data
t_beta <- numeric(length = B)

for (i in 1:B){
  
  # create data frame to pass to lm()-function
  data_sim <- data.frame("y_sim" = y_sim[[i]][2:n],
                         "x" = data$x[2:n],
                         "z" = data$z[2:n],
                         "y_sim_1" = y_sim[[i]][1:n-1])
  
  # estimate model with simulated data
  model_sim <- lm(y_sim ~ x + z + y_sim_1, data = data_sim) 
  
  # obtain statistics for coefficients
  stats_sim <- summary(model_sim)
  
  # calculate and store t-statistic
  t_beta[i] <- stats_sim$coefficients[3,1]/stats_sim$coefficients[3,2]
}


# plot distribution of beta
hist(t_beta, nclass = 40, freq = F)
# add normal distribution
curve(dnorm(x, 0, 1), add = T, col ="blue")

# obtain and print quantiles for critical values
q_boot <- quantile(t_beta, c(0.025, 0.975))
q_norm <- qnorm(c(0.025, 0.975), 0, 1)

print(q_boot)
print(q_norm)




