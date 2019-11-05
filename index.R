rm(list=ls())
library(GMSE)
source('helpers.R')
source('gmse_apply_helpers.R')

source('global_pars2.R')

# sim_old <- gmse_apply(get_res = gmse_paras$get_res,
#                       land_dim_1 = gmse_paras$land_dim_1,
#                       land_dim_2 = gmse_paras$land_dim_2,
#                       land_ownership = gmse_paras$land_ownership,
#                       tend_crops = gmse_paras$tend_crops,
#                       scaring = gmse_paras$scaring,
#                       remove_pr = gmse_paras$remove_pr,         
#                       lambda = gmse_paras$lambda,             
#                       res_death_K = gmse_paras$res_death_K,         
#                       RESOURCE_ini = gmse_paras$RESOURCE_ini,       
#                       manage_target = gmse_paras$manage_target,
#                       res_death_type = gmse_paras$res_death_type,
#                       manager_budget = gmse_paras$manager_budget, 
#                       user_budget = gmse_paras$user_budget,
#                       public_land = gmse_paras$public_land,
#                       stakeholders = gmse_paras$stakeholders, 
#                       res_consume = gmse_paras$res_consume,  
#                       observe_type = gmse_paras$observe_type, 
#                       agent_view = gmse_paras$agent_view,
#                       agent_move = gmse_paras$agent_move,
#                       converge_crit = gmse_paras$converge_crit,
#                       ga_mingen = gmse_paras$ga_mingen)

years = 10
sims = 50

res = list()

for(sim in 1:sims) {
  
  res_year = list()

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

    print(sprintf("Sim %d, year %d", sim, year))
    sim_new <- gmse_apply(get_res = "Full", old_list = sim_old)
    res_year[[year]] = sim_new
    sim_old <- sim_new
    
  }
  
  res[[sim]] = res_year
  
}


par(mfrow=c(1,2))
plot_sims(res, type="resource", sumtype = "none")
plot_sims(res, type="observation", sumtype = "none")









