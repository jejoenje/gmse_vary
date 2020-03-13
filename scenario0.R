rm(list=ls())

### Set and create output folder for scenario run:
scenario_name = "scenario0"
out_path = paste0("./sims/",scenario_name)

library(GMSE)
library(scales)
library(RColorBrewer)
library(doParallel)
source('helpers.R')
source('gmse_apply_helpers.R')
source("build_para_grid.R")

### Override basic parameters according to scenario:
# None needed for scenario0

# Create root output dir for scenario_name, if not create it:
if(!dir.exists(out_path)) {
  dir.create(out_path)
}

n_years = gmse_paras$n_years

# Initialise simulation run with first time step (i.e. time = 0)
sim_old = init_sims(gmse_paras)
# Initialise output for simulation run:
yr_res = init_sim_out(sim_old)

# Loop through nunmber of years
for(i in 1:n_years) {
    
  ### Try to run next time step
  sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)

  ### Check output of next time step; if there are errors (extinctions or no obs), skip to the next sim.
  ### (The following function call should call break() if class(sim_new) == "try-error").
  check_ext = check_gmse_extinction(sim_new, silent = T)
  
  ### So this shoudld only happen if "check_gmse_extinction(sim_new)" has not called break().
  ### So if NOT extinct, append output and reset time step:
  if(check_ext == "ok") {
    yr_res = append_output(sim_new, yr_res)
    sim_old = sim_new
    rm(sim_new)
  } else {
    break()
  }
  
}

# Add parameters to output list:
yr_res$par = gmse_paras

# Save output list:

# Create millisecond timestamp with overprecision:
tstamp = format(Sys.time(), "%Y%m%d%H%M%OS6")
tstamp = sub("\\.","",tstamp)

saveRDS(yr_res, file = paste0(out_path,"/",tstamp,".Rds"))

# To run 30 of this script in parallel:
#  seq 30 | xargs -I{} -P 6 /usr/bin/Rscript tests_for_parallelisation.R


