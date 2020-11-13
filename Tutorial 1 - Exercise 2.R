
setwd("...")
dataset <- read.csv("A1Data2018.csv")

# Part 1
# Transform Variables
dataset_log <- log(dataset)

# Estimate Model
model <- lm(Cost ~ output + Plabor + Pcapital + Pfuel, data = dataset_log)

summary(model)

res <- summary(model)

# Part 2
# Add a new variable to the dataset
set.seed(2)
dataset_log$Noise <- rnorm(145, 0, 1)

# Estimate the new Model including white Noise
model_wn <- lm(Cost ~ output + Plabor + Pcapital + Pfuel + Noise, data = dataset_log)

# Safe summary statistics
res_wn <- summary(model_wn)

# Compare the R squared
res_wn$r.squared > res$r.squared
res_wn$adj.r.squared > res$adj.r.squared

# Part 3
# Split design matrix into two pieces
intercept <- rep(1, nrow(dataset_log))

X_1 <- cbind(intercept, dataset_log$output)
X_2 <- as.matrix(dataset_log[3:5])

y <- dataset_log$Cost

# Calculate P_X1 and M_X1

P_X1 <- X_1 %*% solve(t(X_1) %*% X_1) %*% t(X_1)
M_X1 <- diag(145) - P_X1

# Calculate beta_tilde
beta_tilde <- solve(t(X_2) %*% M_X1 %*% X_2) %*% t(X_2) %*% M_X1 %*% y

print(beta_tilde)
print(model$coefficients)

# Part 4
trace_P_X1 <- sum(diag(P_X1))
print(trace_P_X1)

# Part 5
# Caluclate Matrix P_X

# collect exogenous variables
vars_exo <- as.matrix(dataset_log[2:5])
# add the intercept
X <- cbind(intercept, vars_exo)
# Compute the inverse
XX_inv <- solve(t(X) %*% X)
# caculate P
P <- X %*% XX_inv %*% t(X)

# obtain the diagonal elements and store them in a vector
leverage <- diag(P)

# find the indices of elements in the vecotr with maximum and minimum value 
lev_max <- which.max(leverage)
lev_min <- which.min(leverage)

# remove observation with maximum value and minimum value from dataset
dataset_log_max <- dataset_log[-lev_max, ]
dataset_log_min <- dataset_log[-lev_min, ]

# estimate new models
model_max <- lm(Cost ~ output + Plabor + Pcapital + Pfuel, data = dataset_log_max)
model_min <- lm(Cost ~ output + Plabor + Pcapital + Pfuel, data = dataset_log_min)

# calculate percentage change of coefficients with regards to the old model
change_max <- (model_max$coefficients/model$coefficients-1)*100
change_min <- (model_min$coefficients/model$coefficients-1)*100

print(change_max)
print(change_min)

