rm(list=ls())

scenario_name = "scenario1"
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

# Initialise simulation run with first time step (i.e. time = 0)
sim_old = init_sims(gmse_paras)
# Initialise output for simulation run:
yr_res = init_sim_out(sim_old)

# Loop through nunmber of years
for(i in 1:n_years) {
  
  ### Move resources according to yield, if needed:
  if(gmse_paras$res_move_to_yield==TRUE && gmse_paras$res_move_type==0 ) {
    sim_old$RESOURCES[,5:6] = move_resources_adjusted(sim_old$RESOURCES, sim_old$LAND[,,2], 
                                                      buffer = gmse_paras$res_movement, 
                                                      type = "max")  
  }
  if(gmse_paras$res_move_to_yield==TRUE && gmse_paras$res_move_type!=0) {
    err_found = TRUE
    stop("Invalid res_move_to_yield / res_move_type combination")
  }
  if(gmse_paras$res_move_to_yield==FALSE && gmse_paras$res_move_type==0) {
    err_found = TRUE
    stop("res_move_type set to 0 (no movement) but res_move_to_yield also FALSE - static resources!")
  }
  
  ### Try to run next time step
  sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
  
  ### Check output of next time step; if there are errors (extinctions or no obs), skip to the next sim.
  if(class(sim_new)=="try-error") {
    if(grepl("Extinction", sim_new[1])) {
      print(sprintf("True extinction, skipping to next sim."))
      break()
    } else {
      if(grepl("Error in estimate_abundances", sim_new[1])) {
        print(sprintf("Observed extinction, skipping to next sim."))
        break()
      } else {
        print(sprintf("Observed extinction, skipping to next sim."))
        break()
      }
    }
  } else {
    yr_res = append_output(sim_new, yr_res)
  }
}

# Add parameters to output list:
yr_res$par = gmse_paras

# Save output list:

# Create millisecond timestamp with overprecision:
tstamp = format(Sys.time(), "%Y%m%d%H%M%OS6")
tstamp = sub("\\.","",tstamp)

saveRDS(yr_res, file = paste0(out_path,"/",tstamp,".Rds"))



