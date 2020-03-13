rm(list=ls())
source("helpers.R")
source("gmse_apply_helpers.R")

# Folder for testing
folder = "sims/scenario1"

# Read output files in folder
sims = gmse_rds_summary(folder)

# Number of simulations
length(sims)
# Number of (realised) years per sim:
unlist(lapply(sims, function(x) length(x$pop[,1]) ))

par(mfrow=c(1,2))
gmse_vary_plot(sims, type = "pop", col = "darkred")
gmse_vary_plot(sims, type = "yield", col = "darkgreen")
