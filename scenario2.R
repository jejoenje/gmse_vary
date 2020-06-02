#rm(list=ls())

### Set and create output folder for scenario run:
scenario_name = "scenario2"
out_path = paste0("./sims/",scenario_name)

library(GMSE)
library(scales)
library(RColorBrewer)
library(doParallel)
library(truncnorm)
source('helpers.R')
source('gmse_apply_helpers.R')
source("build_para_grid.R")

### Override basic parameters according to scenario:
# gmse_paras$res_move_type = 0
# gmse_paras$res_move_to_yield = TRUE

# Create root output dir for scenario_name, if not create it:
if(!dir.exists(out_path)) {
  dir.create(out_path)
}

n_years = gmse_paras$n_years

# Initialise simulation run with first time step (i.e. time = 0)
sim_old = init_sims(gmse_paras)

# Reset user budgets (truncated normal):
start_budgets = rtruncnorm(gmse_paras$stakeholders, 
                           a = 0.8*gmse_paras$user_budget, 
                           b = 1.2*gmse_paras$user_budget, 
                           mean = gmse_paras$user_budget, 
                           sd = gmse_paras$user_budget/10)
sim_old$AGENTS[2:nrow(sim_old$AGENTS),17] = start_budgets

# Set manager budget according to user budgets:
sim_old$AGENTS[1,17] = set_man_budget(u_buds = start_budgets, type = "prop", p = gmse_paras$man_bud_prop)

# Initialise output for simulation run:
yr_res = init_sim_out(sim_old)

# Loop through nunmber of years
for(i in 1:n_years) {

  #print(sprintf("Time step %d", i))
    
  ### Move resources according to yield
  #sim_old = move_res(sim_old, gmse_paras)
  
  ### Set next time step's user budgets
  new_b = set_budgets(cur = sim_old, type = "2020", yield_type = gmse_paras$yield_type, yv = gmse_paras$yield_value, scale = FALSE)
  new_b[new_b>10000] = 10000
  new_b[new_b<gmse_paras$minimum_cost] = gmse_paras$minimum_cost
   
  sim_old$AGENTS[2:nrow(sim_old$AGENTS),17] = new_b
  
  ### Set next time step's manager's budget, according to new user budgets
  sim_old$AGENTS[1,17] = set_man_budget(u_buds = new_b, type = "prop", p = gmse_paras$man_bud_prop)

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
#  seq 100 | xargs -I{} -P 6 /usr/bin/Rscript scenario2.R


