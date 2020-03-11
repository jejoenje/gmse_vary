rm(list=ls())

scenario_name = "tests_for_parallelisation"
out_path = paste0("./sims/",scenario_name)

library(GMSE)
library(scales)
library(RColorBrewer)
library(doParallel)
source('helpers.R')
source('gmse_apply_helpers.R')
source("build_para_grid.R")

# Create root output dir for scenario_name, if not create it:
if(!dir.exists(out_path)) {
  dir.create(out_path)
}

n_years = gmse_paras$n_years

sim_old = init_sims(gmse_paras)
yr_res = init_sim_out(sim_old)

for(i in 1:n_years) {
  sim_new = gmse_apply(get_res = "Full", old_list = sim_old)  
  yr_res = append_output(sim_new, yr_res)
}

# Add parameters to output list:
yr_res$par = gmse_paras

# Save output list:

# Create millisecond timestamp with overprecision:
tstamp = format(Sys.time(), "%Y%m%d%H%M%OS6")
tstamp = sub("\\.","",tstamp)

saveRDS(yr_res, file = paste0(out_path,"/",tstamp,".Rds"))



