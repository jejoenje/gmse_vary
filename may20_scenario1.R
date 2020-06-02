rm(list=ls())

library(GMSE)
source("helpers.R")
source("gmse_vary_helpers.R")
source("parasMay2020.R")

### Setup output folder:
# Output scenario name:
scenario_name = "may20_scenario1_04"

### Scenario-specific parameters
ov = 0 # == ownership_var

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
                     land_dim_1 = dim_1,
                     land_dim_2 = dim_2,
                     land_ownership = lo,
                     res_birth_type = rbt,
                     res_death_type = rdt,
                     lambda = l,
                     remove_pr = rpr,
                     res_birth_K = rbK,
                     res_death_K = rdK,
                     manage_target = mt,
                     RESOURCE_ini = r_ini,
                     stakeholders = s,
                     res_consume = r_cons,
                     agent_view = a_view,
                     times_observe = t_obs,
                     scaring = scare,
                     culling = cull,
                     tend_crops = tend_crop,
                     tend_crop_yld = tcy,
                     minimum_cost = mc,
                     ownership_var = ov
                     )


# Loop through nunmber of years
for(i in 2:n_years) {

  ### Move resources according to yield   #### NOT USED FOR NOW
  #sim_old = move_res(sim_old, gmse_paras)
  
  ### Try to run next time step
  sim_new = try({gmse_apply(old_list = sims[[i-1]], get_res = "Full")},silent = T)
  ### Check output of next time step; if there are errors (extinctions or no obs), skip to the next sim.
  ### (The following function call should call break() if class(sim_new) == "try-error").
  check_ext = check_gmse_extinction(sim_new, silent = T)
  ### If NOT extinct, append output and reset time step:
  if(check_ext == "ok") {
    sims[[i]] = sim_new
    rm(sim_new)
  } else {
    break()
  }
  
}

### Save simulation:
outfile = paste0(out_folder,out_name,".Rds")
saveRDS(sims, file = outfile)
print(sprintf("Done %d", length(list.files(out_folder))))


