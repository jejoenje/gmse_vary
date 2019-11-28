### Null model simulation runs
### 

rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source("helpers.R")
source("gmse_apply_helpers.R")
source("sims/global_paras.R")

###################################
###################################
###################################
sim_set_name = "nullModel-YTB4"
##########################
##############################  
##################################
###################################
  
# ###
# ###### Temporary override of global para (for tests - need to remove!)    <---------------------
# gmse_paras$public_land = 0
# gmse_paras$sims = 10
# gmse_paras$years = 10
# 
# sims = gmse_paras$sims
# years = gmse_paras$years

### Initialise simulations (set output folders and paras etc) 
init_sims(sim_set_name)

### Initialise outputs
init_out(s = sims, y = years, users=gmse_paras[["stakeholders"]])

### Manually set "yield-to-budget" value and save with paras:
gmse_paras$yield_value = 0.2

for(sim in 1:sims) {
  
  sim_old <- gmse_apply(get_res = gmse_paras$get_res,
                        land_dim_1 = gmse_paras$land_dim_1,
                        land_dim_2 = gmse_paras$land_dim_2,
                        land_ownership = gmse_paras$land_ownership,
                        tend_crops = gmse_paras$tend_crops,
                        scaring = gmse_paras$scaring,
                        remove_pr = gmse_paras$remove_pr,         
                        lambda = gmse_paras$lambda,             
                        res_death_K = gmse_paras$res_death_K,         
                        RESOURCE_ini = gmse_paras$RESOURCE_ini,       
                        manage_target = gmse_paras$manage_target,
                        res_death_type = gmse_paras$res_death_type,
                        manager_budget = gmse_paras$manager_budget, 
                        user_budget = gmse_paras$user_budget,
                        public_land = gmse_paras$public_land,
                        stakeholders = gmse_paras$stakeholders, 
                        res_consume = gmse_paras$res_consume,  
                        observe_type = gmse_paras$observe_type, 
                        agent_view = gmse_paras$agent_view,
                        agent_move = gmse_paras$agent_move,
                        converge_crit = gmse_paras$converge_crit,
                        ga_mingen = gmse_paras$ga_mingen)
  
  ### Initialise user budgets (random sample but uniform)
  u_bud_sample = sample_budgets_ManyPoorFewRich()
  sim_old$AGENTS[2:(gmse_paras[["stakeholders"]]+1),17] = u_bud_sample
  
  ### Initialise manager budget (with method man_bud_type) 
  sim_old$AGENTS[1,17] = set_man_budget(u_bud_sample, 
                                        type = gmse_paras$man_bud_type)
  sim_old$manager_budget = sim_old$AGENTS[1,17]
  
  ### Initialise custom land placement (this is independent of budget other than it will affect yield, and thus budgets).
  ### Type = "equal" is the default and distributes non-public land evenly among users.
  ### Type = "oneRich" takes a given mean proportion (hi_frac) and allocates this to one single user. Note there is some error
  ###  around this (beta-distribution) with upper and lower bounds optionally set by up_frac and lo_frac.
  sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                               type = gmse_paras$land_type, 
                               hi_frac = gmse_paras$land_type_max_frac)

  for(year in 1:years) {
    
    sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
    
    if(class(sim_new)=="try-error") {
      store_dat(sim_new, s = sim, y = year, type = "ext")     ### Record extinction
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
      print(sprintf("Sim %d, year %d", sim, year))
      
      ### Set next budgets according to yields:
      nxt_budgets = set_budgets(prv = sim_old,                      # To extract remaining budget from previous
                                nxt = sim_new,                      # Current year sim results
                                yv = gmse_paras$yield_value,        # Monetary return per unit yield
                                yield_type = gmse_paras$ytb_type)   # Function translating yield into return
      sim_new$AGENTS[2:(gmse_paras[["stakeholders"]]+1),17] = nxt_budgets
      
      sim_new$AGENTS[1,17] = set_man_budget(nxt_budgets, 
                                            type = gmse_paras$man_bud_type)
      sim_new$manager_budget = sim_new$AGENTS[1,17]
      
      store_dat(sim_new, s = sim, y = year)
      
      sim_old <- sim_new
    }
  }
}

plot_gmse_sims(POP, USR)

write.csv(POP, sprintf("%sPOP_%s.csv", outpath, outidx), row.names=F)
write.csv(USR, sprintf("%sUSR_%s.csv", outpath, outidx), row.names=F)
write.csv(EXT, sprintf("%sEXT_%s.csv", outpath, outidx), row.names=F)





