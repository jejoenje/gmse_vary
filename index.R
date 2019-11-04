rm(list=ls())
library(GMSE)
source('helpers.R')
source('gmse_apply_helpers.R')
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

years = 25
sims = 2

res = list()

for(sim in 1:sims) {
  
  res_year = list()
  
  for(year in 1:years) {

    print(sprintf("Sim %d, year %d", sim, year))
    sim_new <- gmse_apply(get_res = "Full", old_list = sim_old)
    res_year[[year]] = sim_new
    sim_old <- sim_new
    
  }
  
  res[[sim]] = res_year
  
}

plot_sims(res, type="resource", sumtype = "none")











