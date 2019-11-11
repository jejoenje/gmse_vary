rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source('helpers.R')
source('gmse_apply_helpers.R')

source('global_pars2.R')

years = 50
sims = 10

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
    } else {
      print(sprintf("Sim %d, year %d", sim, year))
      res_year[[year]] = sim_new
      sim_old <- sim_new
    }
  }
  
  res[[sim]] = res_year
  
}

check_len = lapply(res, function(x) lapply(x, len))


### If not all of these are == years, some extinctions occurred.
unlist(lapply(res, len))


unlist(lapply(res, function(x) lapply(x, class)  ))

checkState = data.frame(sim = rep(1:sims, each=years), 
                        year = rep(1:years,sims), 
                        state = unlist(lapply(res, function(x) lapply(x, class)  )))
checkState

 
par(mfrow=c(2,2))
plot_resource(res, type="resources", sumtype = "none", ylim = c(0, 1400))
plot_resource(res, type="observations", sumtype = "none", ylim = c(0, 1400))
plot_actions(res, type = "mean")
plot_yield(res, type = "all")

# 
# get_user_data(res, "observations")





