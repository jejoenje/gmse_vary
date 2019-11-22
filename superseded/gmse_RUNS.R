rm(list=ls())
library(GMSE)
library(RColorBrewer)
source('helpers.R')
source('global_pars.R')
source('gmse_iter.R')
source('gmse_iter_plot.R')

u_budget = 1000

sim_old <- gmse_apply(get_res = gmse_paras$get_res,                  # "Full" 
                      land_dim_1 = gmse_paras$land_dim_1,            # 100
                      land_dim_2 = gmse_paras$land_dim_2,            # 100
                      land_ownership = gmse_paras$land_ownership,    # TRUE
                      tend_crops = gmse_paras$tend_crops,            # TRUE
                      tend_crop_yld = gmse_paras$tend_crop_yld,      # 0.5
                      scaring = gmse_paras$scaring,                  # TRUE  
                      remove_pr = gmse_paras$remove_pr,              # 0.2
                      lambda = gmse_paras$lambda,                    # 5000
                      res_death_K = K,                               # 5000      
                      RESOURCE_ini = N,                              # 5000
                      manage_target = target,                        # 5000
                      res_death_type = gmse_paras$res_death_type,    # 3
                      manager_budget = gmse_paras$manager_budget,    # 1000
                      user_budget = u_budget,                                # varies
                      public_land = gmse_paras$public_land,          # 0.1
                      stakeholders = sholders,                       # 8
                      res_consume = gmse_paras$res_consume,          # 0.035
                      res_birth_K = birth_K,                         # 6000
                      observe_type = gmse_paras$observe_type,        # 3
                      agent_view = gmse_paras$agent_view,            # 1
                      agent_move = gmse_paras$agent_move,            # 25
                      converge_crit = gmse_paras$converge_crit,      # 0.02
                      ga_mingen = gmse_paras$ga_mingen)              # 100


#assign("sim_old", sim_old, envir = globalenv())
test = gmse_itr(sim_old = sim_old, yrs = 200)

gmse_iter_plot(gmse_iter_dat = test)
