rm(list=ls())
source("helpers.R")
source("gmse_apply_helpers.R")

# Scenario 0:
folder = "sims/scenario0"
# Read output files in folder
sims0 = gmse_rds_summary(folder)
# Number of simulations
length(sims0)
# Number of (realised) years per sim:
unlist(lapply(sims0, function(x) length(x$pop[,1]) ))

par(mfrow=c(5,3))
par(mar = c(4,4,0.5,0.5))
gmse_vary_plot(sims0, type = "pop", col = "darkred", ylab = "Population", xlab = "Time step")
gmse_vary_plot(sims0, type = "yield", col = "darkgreen", sumtype = "median", ylab = "Yield", xlab = "Time step")
gmse_vary_plot(sims0, type = "budget", col = "darkblue", sumtype = "median", ylab = "Budget", xlab = "Time step")

