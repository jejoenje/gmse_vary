rm(list=ls())

library(GMSE)
library(truncnorm)
source("gmse_vary_helpers.R")
source("parasMay2020.R")

### Setup output folder:
# Output scenario name:
scenario_name = "may20_scenario2"

# Setup/check output folder:
out_folder = paste0("sims/",scenario_name,"/")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
}

### START SIMULATION RUN
# Create an output name (timestamp) for this sim
out_name = gsub("\\.", "",format(Sys.time(),"%Y%m%d%H%M%OS6"))

### Start empty output list:
sims = list()

# Initialise simulation run with first time step (i.e. time = 0)
#print("Time step 1")
sims[[1]] = gmse_apply(get_res = "Full",
                     land_ownership = land_ownership,
                     res_birth_type = res_birth_type,
                     res_death_type = res_death_type,
                     lambda = lambda,
                     remove_pr = remove_pr,
                     res_birth_K = res_birth_K,
                     res_death_K = res_death_K,
                     manage_target = manage_target,
                     RESOURCE_ini = RESOURCE_ini,
                     stakeholders = stakeholders,
                     res_consume = res_consume,
                     agent_view = agent_view,
                     scaring = scaring,
                     culling = culling,
                     tend_crops = tend_crops,
                     tend_crop_yld = tend_crop_yld,
                     minimum_cost = minimum_cost
                     )

### Set user budgets according to yield:
start_budgets = set_budgets(cur = sims[[1]], yield_type = "linear", yv = yield_value)
sims[[1]]$AGENTS[2:(stakeholders+1),17] = start_budgets

# Set manager budget according to user budgets:
sims[[1]]$AGENTS[1,17] = set_man_budget(u_buds = start_budgets, type = "prop", p = man_bud_prop)

# Output:
print_sim_status(1, sims[[1]])

# Loop through nunmber of years
for(i in 2:n_years) {

  ### Move resources according to yield   
  #sim_old = move_res(sim_old, gmse_paras)
  
  ### Try to run next time step
  sim_new = try({gmse_apply(old_list = sims[[i-1]], get_res = "Full")},silent = T)
  ### Check output of next time step; if there are errors (extinctions or no obs), skip to the next sim.
  ### (The following function call should call break() if class(sim_new) == "try-error").
  check_ext = check_gmse_extinction(sim_new, silent = T)
  
  ### So this shoudld only happen if "check_gmse_extinction(sim_new)" has not called break().
  ### So if NOT extinct, append output and reset time step:
  if(check_ext == "ok") {
    
    sims[[i]] = sim_new

    ### Re-set user budgets according to current yield
    new_b = set_budgets(cur = sims[[i]], 
                        yield_type = yield_type, 
                        yv = yield_value)
    new_b[new_b>10000] = 10000
    new_b[new_b<minimum_cost] = minimum_cost
    sims[[i]]$AGENTS[2:(stakeholders+1),17] = new_b
    
    ### Set next time step's manager's budget, according to new user budgets
    sims[[i]]$AGENTS[1,17] = set_man_budget(u_buds = new_b, type = "prop", p = man_bud_prop)
    
    # Output:
    print_sim_status(i, sims[[i]])
    
    rm(sim_new)
  } else {
    break()
  }
  
}

### Save simulation:
outfile = paste0(out_folder,out_name,".Rds")
saveRDS(sims, file = outfile)


