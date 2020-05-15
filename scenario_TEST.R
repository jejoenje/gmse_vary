  rm(list=ls())
  
  library(GMSE)
  library(scales)
  library(RColorBrewer)
  library(truncnorm)
  source('helpers.R')
  source('gmse_apply_helpers.R')
  
  source("build_para_grid.R")
  
  ### Overrides for GMSE update:
  # gmse_paras$consume_surv = 10
  # gmse_paras$consume_repr = 3
  # gmse_paras$times_feeding = 1
  gmse_paras$res_birth_type = 2
  gmse_paras$res_death_type = 2
  gmse_paras$lambda = 0.3
  gmse_paras$remove_pr = 0
  gmse_paras$res_birth_K = 1e+05
  gmse_paras$res_death_K = 2000
  gmse_paras$manage_target = 1000
  gmse_paras$RESOURCE_ini = 1000
  gmse_paras$stakeholders = 4
  gmse_paras$user_budget = 1000
  gmse_paras$manager_budget = 1000
  
  gmse_paras$res_consume = 0.5
  gmse_paras$agent_view = 50
  gmse_paras$times_observe = 5
  
  # Actions
  gmse_paras$scaring = TRUE
  gmse_paras$culling = TRUE
  gmse_paras$tend_crops = TRUE
  gmse_paras$tend_crop_yld = 0.6  # 0.475
  
  gmse_paras$n_years = 50
  
  # Initialise simulation run with first time step (i.e. time = 0)
  sim_old = init_sims(gmse_paras)
  
  # # Reset user budgets (truncated normal):
  # start_budgets = rtruncnorm(gmse_paras$stakeholders, 
  #                            a = 0.8*gmse_paras$user_budget, 
  #                            b = 1.2*gmse_paras$user_budget, 
  #                            mean = gmse_paras$user_budget, 
  #                            sd = gmse_paras$user_budget/10)
  # sim_old$AGENTS[2:nrow(sim_old$AGENTS),17] = start_budgets
  
  # Set manager budget according to user budgets:
  # sim_old$AGENTS[1,17] = set_man_budget(u_buds = start_budgets, type = "prop", p = gmse_paras$man_bud_prop)
  
  # Initialise output for simulation run:
  yr_res = init_sim_out(sim_old)
  
  sims = list()
  sims[[1]] = sim_old
  
  # Loop through nunmber of years
  for(i in 1:gmse_paras$n_years) {
    
    print(sprintf("Time step %d", i))
    
    ### Move resources according to yield
    #sim_old = move_res(sim_old, gmse_paras)
    
    ### Set next time step's user budgets
    # new_b = set_budgets(cur = sim_old, type = "2020", yield_type = gmse_paras$yield_type, yv = gmse_paras$yield_value, scale = FALSE)
    # new_b[new_b>10000] = 10000
    # new_b[new_b<gmse_paras$minimum_cost] = gmse_paras$minimum_cost
    # sim_old$AGENTS[2:nrow(sim_old$AGENTS),17] = new_b
   
    ### Set next time step's manager's budget, according to new user budgets
    # sim_old$AGENTS[1,17] = set_man_budget(u_buds = new_b, type = "prop", p = gmse_paras$man_bud_prop)
    
    ### Try to run next time step
    sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
    
    ### Check output of next time step; if there are errors (extinctions or no obs), skip to the next sim.
    ### (The following function call should call break() if class(sim_new) == "try-error").
    check_ext = check_gmse_extinction(sim_new, silent = T)
    
    ### So this shoudld only happen if "check_gmse_extinction(sim_new)" has not called break().
    ### So if NOT extinct, append output and reset time step:
    if(check_ext == "ok") {
      sims[[i]] = sim_new
      yr_res = append_output(sim_new, yr_res)
      sim_old = sim_new
      rm(sim_new)
    } else {
      break()
    }
    
  }
  
  # Add parameters to output list:
  yr_res$par = gmse_paras
  
  # Add land ownership matrix to output list:
  yr_res$land = sim_old$LAND[,,3]
  
  #sims = list(yr_res)
  yr_res = list(yr_res)
  
  opar = par()
  
  layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  
  gmse_vary_plot(yr_res, type = "pop", col = "darkred", 
                 ylab = "Population", xlab = "Time step")
  gmse_vary_plot(yr_res, type = "yield_usermean", col = "darkgreen", sumtype = "median", 
                 ylab = "Yield", xlab = "Time step")
  gmse_vary_plot(yr_res, type = "actions_sum")
  