rm(list=ls())

library(GMSE)

### Pause a random number of seconds to avoid clashes
Sys.sleep(runif(1, 0.5, 1))

### Generate a name for output, based on ms time:
outname = format(Sys.time(), "%Y%m%d%H%M%OS6")
outname = gsub("\\.", "", outname)

###### Pick parameters

### Load parameter grid
load("sims/gmse_vary_06/para_grid.Rdata")
### Find which parameter combination is "next"; i.e. which one is the first yet to be done and
###  is not yet due to be completed by anything currently running:
cpar = min(which((par1$done<par1$J) & ((par1$J-par1$done) > par1$act)))
### Set this first one to "active":
par1$act[cpar] = par1$act[cpar]+1
### Save parameter grid:
save(par1, file = "sims/gmse_vary_06/para_grid.Rdata")

###### Run simulations

### Run simulations with chosen parameter set
sims = gmse(time_max = par1[cpar,"Y"], 
     RESOURCE_ini = 1000, 
     lambda = 0.3, 
     res_death_K = 2000,
     res_consume = 0.5, 
     manage_target = 1000, 
     land_ownership = TRUE,
     scaring = TRUE,
     tend_crops = TRUE,
     tend_crop_yld = 0.5,
     usr_yld_budget = par1[cpar,"uyb"], 
     man_yld_budget = par1[cpar,"myb"], 
     stakeholders = par1[cpar,"s"],
     ownership_var = par1[cpar,"ov"],
     manager_budget = par1[cpar,"mb"], 
     user_budget = par1[cpar,"ub"], 
     agent_view = par1[cpar,"av"], 
     times_observe = par1[cpar,"to"])

### Add current parameters to output list:
sims[[length(sims)+1]] = par1[cpar,]

### Save output list:
saveRDS(sims, file = paste0("sims/gmse_vary_06/",outname,".Rds"))