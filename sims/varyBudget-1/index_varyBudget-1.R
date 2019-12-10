### VArying budget across users across sims (constant within sims).
### Budget scale 1 (stakeholders at most 1*budget of manager)
### Land ownership equal.

rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source("helpers.R")### VArying budget across users across sims (constant within sims).
### Budget scale 1 (stakeholders at most 1*budget of manager)
### Land ownership equal.

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
sim_set_name = "varyBudget-1"
##########################
##############################
##################################
###################################

init_sims(sim_set_name)


### Init empty list
res = list()

for(sim in 1:sims) {
  
  res_year = as.list(rep(NA, years))
  
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
  
  ### Set budgets for users:
  K = gmse_paras$stakeholders
  for(i in 1:K) {
    sim_old$AGENTS[2:nrow(sim_old$AGENTS),17] = sample_budgets_ManyPoorFewRich(nstakeholders = K, 
                                                                               manager_budget = gmse_paras$manager_budget,
                                                                               scale = budget_scale)
  }
  
  for(year in 1:years) {
    
    sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
    if(class(sim_new)=="try-error") {
      if(grepl("Extinction", sim_new[1])) {
        print(sprintf("True extinction, skipping to next sim."))
        res_year[year:years] = "Extinction (true)"
        break()
      } else {
        if(grepl("Error in estimate_abundances", sim_new[1])) {
          print(sprintf("Observed extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (observed)"
          break()
        } else {
          print(sprintf("Observed extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (observed, other error)"
          break()
        }
      }
    } else {sample_budgets_ManyPoorFewRich
      print(sprintf("Sim %d, year %d", sim, year))
      res_year[[year]] = trunc_res(sim_new)
      sim_old <- sim_new
    }
  }
  
  res[[sim]] = res_year
  
source("gmse_apply_helpers.R")
source("sims/global_paras.R")

###################################
###################################
###################################
sim_set_name = "varyBudget-1"
##########################
##############################
##################################
###################################

init_sims(sim_set_name)


### Init empty list
res = list()

for(sim in 1:sims) {
  
  res_year = as.list(rep(NA, years))
  
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
  
  ### Set budgets for users:
  K = gmse_paras$stakeholders
  for(i in 1:K) {
    sim_old$AGENTS[2:nrow(sim_old$AGENTS),17] = sample_budgets_ManyPoorFewRich(nstakeholders = K, 
                                                                               manager_budget = gmse_paras$manager_budget,
                                                                               scale = budget_scale)
  }
  
  for(year in 1:years) {
    
    sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
    if(class(sim_new)=="try-error") {
      if(grepl("Extinction", sim_new[1])) {
        print(sprintf("True extinction, skipping to next sim."))
        res_year[year:years] = "Extinction (true)"
        break()
      } else {
        if(grepl("Error in estimate_abundances", sim_new[1])) {
          print(sprintf("Observed extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (observed)"
          break()
        } else {
          print(sprintf("Observed extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (observed, other error)"
          break()
        }
      }
    } else {sample_budgets_ManyPoorFewRich
      print(sprintf("Sim %d, year %d", sim, year))
      res_year[[year]] = trunc_res(sim_new)
      sim_old <- sim_new
    }
  }
  
  res[[sim]] = res_year
  saveRDS(res[[sim]], sprintf("%ssim%04d_%s_%s.Rds", outpath, sim, outidx, sim_set_name))
}

### T
format(object.size(res), units = "auto")
for(i in 1:sims) {
  for(j in 1:years) {
    res[[i]][[j]]$LAND = NULL
    res[[i]][[j]]$resource_array = NULL
    res[[i]][[j]]$observation_array = NULL
    res[[i]][[j]]$manager_array = NULL
    res[[i]][[j]]$user_array = NULL
  }
}
format(object.size(res), units = "auto")

y_lims = c(bufRange(min(extract_gmse(res, "resources"), na.rm=T), end = "lo"),
           bufRange(max(extract_gmse(res, "resources"), na.rm=T), end = "hi"))
par(mfrow=c(1,1))
tiff(sprintf("%s%s/resources.tiff", outdir, outidx))
plot_resource(res, type="resources", sumtype = "none", ylim = y_lims)
dev.off()
tiff(sprintf("%s%s/observations.tiff", outdir, outidx))
plot_resource(res, type="observations", sumtype = "none", ylim = y_lims)
dev.off()
tiff(sprintf("%s%s/actions.tiff", outdir, outidx))
plot_actions(res, type = "mean")
dev.off()
tiff(sprintf("%s%s/yield.tiff", outdir, outidx))
plot_yield(res, type = "all")
dev.off()

par(mfrow=c(2,2))
plot_resource(res, type="resources", sumtype = "none", ylim = y_lims)
plot_resource(res, type="observations", sumtype = "none", ylim = y_lims)
plot_actions(res, type = "mean")
plot_yield(res, type = "all")


