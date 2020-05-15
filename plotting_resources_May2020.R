rm(list=ls())
library(GMSE)

source("gmse_vary_helpers.R")

folder = "sims/may20_scenario2"

out_files = list.files(folder)
out_files = out_files[grep(".Rds", out_files)]
out_files = paste0(folder, "/",out_files)

sims = list()
for(i in 1:length(out_files)) {
  sims[[i]] = readRDS(out_files[i])
}

out = sims

sim1 = sims[[1]]

sim1[[1]]$LAND[[,,]]