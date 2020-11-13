library(readr)
library(systemfit)

setwd("...")

demand_supply_data <- read_csv("ds_data.csv")

# Estimate Demand Equation with 3SLS
eqDemand <- quantity ~ price + income + demand_shifts
eqSupply <- quantity ~ price + input_1 + input_2
system <- list( demand = eqDemand, supply = eqSupply )


inst <- ~ income + demand_shifts + input_1 + input_2

fit2sls <- systemfit( system, "2SLS", inst = inst, data = demand_supply_data)

summary(fit2sls)


fit3sls <- systemfit( system, "3SLS", inst = inst, data = demand_supply_data)

summary(fit3sls)




