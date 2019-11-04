rm(list=ls())
library(GMSE)
source('helpers.R')
source('global_pars.R')

sim_old <- gmse_apply(get_res = gmse_paras$get_res,
                      land_dim_1 = gmse_paras$land_dim_1,
                      land_dim_2 = gmse_paras$land_dim_2,
                      land_ownership = gmse_paras$land_ownership,
                      tend_crops = gmse_paras$tend_crops,
                      scaring = gmse_paras$scaring,
                      remove_pr = gmse_paras$remove_pr,         
                      lambda = gmse_paras$lambda,             
                      res_death_K = K,         
                      RESOURCE_ini = N,       
                      manage_target = target,
                      res_death_type = gmse_paras$res_death_type,
                      manager_budget = gmse_paras$manager_budget, 
                      user_budget = u_budget,
                      public_land = gmse_paras$public_land,
                      stakeholders = sholders, 
                      res_consume = gmse_paras$res_consume,  
                      res_birth_K = birth_K,
                      observe_type = gmse_paras$observe_type, 
                      agent_view = gmse_paras$agent_view,
                      agent_move = gmse_paras$agent_move,
                      converge_crit = gmse_paras$converge_crit,
                      ga_mingen = gmse_paras$ga_mingen)

years = 3
sims = 4

res = list()

for(sim in 1:sims) {
  
  res_year = list()
  
  for(year in 1:years) {

    sprintf("Sim %d, year %d", sim, year)
    sim_new <- gmse_apply(get_res = "Full", old_list = sim_old)
    res_year[[year]] = sim_new
    sim_old <- sim_new
    
  }
  
  res[[sim]] = res_year
  
}



plot_sims = function(gmse_res, type="resource1", sumtype="mean") {
  no_sims = len(res)
  no_years = length(res[[1]][])
  
  ### type="resource1" plots resource and resource count
  
  if(type=="resource1") {
    
    
    
    sum_res = function(all_dat, extract = "resource_results") {
      no_sims = len(all_dat)
      no_years = length(all_dat[[1]][])
      dat = matrix(NA, nrow=no_sims, ncol=no_years)
      for(i in 1:no_sims) {
        for(j in 1:no_years) {
          dat[i,j] = 
        }
      }
    }
    
    # Extract resource_results and observation results, and reconstruct a time series for X
    y_res = unlist(lapply(gmse_res, function(x) x$basic_output$resource_results))
    y_obs = unlist(lapply(gmse_res, function(x) x$basic_output$observation_results))
    x = 1:no_years
    
    # Set upper/lower bounds for Y: 5% plus/minus the max/min for combined y_res and y_obs
    y_max = ceiling(max(c(y_obs, y_res))+max(c(y_obs, y_res))*0.05)
    y_min = floor(min(c(y_obs, y_res))-min(c(y_obs, y_res))*0.05)
    
    # Reset upper/lower limits if the manage_target line isn't within range.
    manage_target = gmse_res[[1]]$manage_target
    if( manage_target > y_max ) y_max = manage_target
    if( manage_target < y_min ) y_min = manage_target
    
    par(mfrow=c(1,1))
    plot(x, y_res, type="l", lwd = 2, ylim = c(y_min, y_max))
    lines(x, y_obs, lwd = 1, col = "grey")
    abline(h = manage_target, col="red", lty="dashed")
    
  }
}

plot_sims(res)



