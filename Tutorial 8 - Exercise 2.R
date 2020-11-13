library(readr)
library(systemfit)

# Import data
setwd("...")
demand_supply_data <- read_csv("ds_data.csv")

# Define System of equations
eqDemand <- quantity ~ price + income + demand_shifts
eqSupply <- quantity ~ price + input_1 + input_2
system <- list( demand = eqDemand, supply = eqSupply )

# Define Instruments
inst <- ~ income + demand_shifts + input_1 + input_2

# Estimate Model with 2SLS
fit2sls <- systemfit( system, "2SLS", inst = inst, data = demand_supply_data)

summary(fit2sls)

# Estimate Model with 3SLS
fit3sls <- systemfit( system, "3SLS", inst = inst, data = demand_supply_data)

summary(fit3sls)




