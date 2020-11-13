################################################################################
################ Simulation of Asymptotic Unbiasdness of OLS ###################
################################################################################

## Set simulation parameters

# True value of the DGP
alpha <- 0.5
# sample size
N <- 50 
# Initial value
y_0 <- 0


## simulation of data from data generating process

# empty vector to hold time series entries
y_t <- numeric(length = 50)

# first entry of y_t
y_t[1] <- y_0 + rnorm(1, 0, 1)

# create entries for t>0 based on DGP
for (i in 2:N){
  y_t[i] <- alpha*y_t[i-1] + rnorm(1, 0, 1)
}

# estimate the coefficient using OLS
ar_est <- ar(y_t, order.max = 1, aic = FALSE, method = "ols")

# Bias
bias <- ar_est$ar - alpha

#Print results
print(ar_est$ar)
print(bias)


## Run Simulation 5000 times

# Number of simulations
M <- 5000

# empty vector for estimated coefficients 
est_coefs <- numeric(length = M)

# Loop over M=5000 
for (j in 1:M){
  y_t <- numeric(length = 50)
  y_t[1] <- y_0 + rnorm(1, 0, 1)
  
  for (i in 2:N){
    y_t[i] <- alpha*y_t[i-1] + rnorm(1, 0, 1)
  }
  
  ar_est <- ar(y_t, order.max = 1, aic = FALSE, method = "ols")
  
  # Store estimated coefficients in vector est_coefs
  est_coefs[j] <- ar_est$ar
}


## Run Simulation for different sample sizes

# vector with different sample sizes
N_set <- c(10, 50, 100, 500, 1000)

# List to hold paramters for different sample sizes
sim_results <- list()


# Loop over each sample size in N_set
for (k in 1:length(N_set)){
  
  est_coefs <- numeric(length = M)
  
  for (j in 1:M){
    y_t <- numeric(length = N_set[k])
    y_t[1] <- y_0 + rnorm(1, 0, 1)
    
    for (i in 2:N_set[k]){
      y_t[i] <- alpha*y_t[i-1] + rnorm(1, 0, 1)
    }
    
    ar_est <- ar(y_t, order.max = 1, aic = FALSE, method = "ols")
    est_coefs[j] <- ar_est$ar
  }
  
  sim_results[[k]] <- est_coefs
}


# Empty vector to hold Bias for each sample size
sim_bias <- numeric(length = 5)

# Loop over each set of coefficients in sim_results
for(i in 1:length(sim_results)){
  
  # Bias of the mean 
  sim_bias[i] <- mean(sim_results[[i]] - alpha)
}


# Plot Simulation Results
xtick <- seq(1,5)
xlabels <- as.character(N_set)
plot(sim_bias, type = "l", xaxt = "n", lwd = 2, main = "Finite Sample Bias",
     ylab = "Bias", xlab = "Sample Size N")
grid()
axis(side=1, labels = FALSE, tick = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xlabels, pos = 1, xpd = TRUE)



################################################################################
################## Extension to different values for alpha #####################
################################################################################

# vector with different values for alpha
alpha_set <- c(0.1, 0.5, 0.9)

# list to store simulation results for different values of alpha
sim_results_out <- list()

# Loop over different values for alpha 
for (l in 1:length(alpha_set)){
  
  sim_results_in <- list()
  alpha <- alpha_set[l]
  
  for (k in 1:length(N_set)){
    
    est_coefs <- numeric(length = M)
    
    for (j in 1:M){
      y_t <- numeric(length = 50)
      y_t[1] <- y_0 + rnorm(1, 0, 1)
      
      for (i in 2:N_set[k]){
        y_t[i] <- alpha*y_t[i-1] + rnorm(1, 0, 1)
      }
      
      ar_est <- ar(y_t, order.max = 1, aic = FALSE, method = "ols")
      est_coefs[j] <- ar_est$ar
    }
    
    sim_results_in[[k]] <- est_coefs
  }
  
  sim_bias_in <- numeric(length = 5)
  
  for(i in 1:length(N_set)){
    sim_bias_in[i] <- mean(sim_results_in[[i]] - alpha_set[l])
  }
  
  sim_results_out[[l]] <- sim_bias_in 
}


# Plot all Simulation results in the same plot
plot(sim_results_out[[3]], type = "l", xaxt = "n", lwd = 2, 
     main = "Finite Sample Bias", ylab = "Bias", xlab = "Sample Size N")
lines(sim_results_out[[1]], col = "blue", lwd = 2)
lines(sim_results_out[[2]], col = "red", lwd = 2)
grid()
axis(side=1, labels = FALSE, tick = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xlabels, pos = 1, xpd = TRUE)
legend("bottomright", legend = c("alpha = 0.9", "alpha = 0.5", "alpha = 0.1"),
       col = c("black", "red", "blue"), lwd = 2)