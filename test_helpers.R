# Helpers for tests

### This is a helper function to run a single gmse_apply call as a first time step, using a given list of input parameters.
### Only intended as a convenience function to ensure simulation scripts can be relatively "clean".
init_sims = function(gmse_paras = gmse_paras) {
  gmse_apply(get_res = gmse_paras$get_res,
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
                        ga_mingen = gmse_paras$ga_mingen,
                        res_movement = gmse_paras$res_movement,
                        res_move_type = gmse_paras$res_move_type)
}


### Sets up an output list structure to save the output of a series of simulation time steps. For a series of 
###  simulations, there would be an output like this for each simualtion.
### This is intended as setting up the output for time step 0 or time step 1, as a starting point for a run of time steps.
### This function takes a single input (dat) which is expected to be the output of a single gmse_apply() call.
### It summarises the results in this using extractUser() calls for yield, budget, crops, kills, scares, noact.
### In addition it extract resource population results (both observed and actual, as well as the landscape position
###  of all resources and yield values for each landscape cell). 
### Output is a list with 9 elements. Seven of these (yield, budget, crops, kills, scares, 
###  noact and pop) are dataframes with a new line for each time step, and themselves are outputs of extractUser() 
###  function calls. The last two list elements are matrixes of resource positions in the landscape and yield values in 
###  the landscape (so a single matrix per new time step).
init_sim_out = function(dat) {
  out_i = list(yield = t(as.data.frame(extractUser(dat, "yield"))),
               budget = t(as.data.frame(extractUser(dat, "budget"))),
               crops =  t(as.data.frame(extractUser(dat, "crops"))),
               kills = t(as.data.frame(extractUser(dat, "kills"))),
               scares = t(as.data.frame(extractUser(dat, "scares"))),
               noact =  t(as.data.frame(extractUser(dat, "noact"))),
               pop = t(as.data.frame(c(dat$basic_output$resource_results, 
                                       dat$basic_output$observation_results))),
               res = list(dat$RESOURCES),
               land_yield = list(dat$LAND[,,2]))
  
  return(out_i)
}

### This appends the results from a simulation for a given time step (WITHIN A SIMULATION, SO NOT TAKING ACCOUNT 
###  OF DIFFERENCES BETWEEN SIMS) and appends the results to an existing list.
### New year's results (new) should be given as the "full" output of a gmse_apply() run.
### Existing simulation results should be given at the output of either this function, or the output of init_sim_out().
### In any case, 'existing' is expected to be a list with 9 elements. Seven of these (yield, budget, crops, kills, scares, 
###  noact and pop) are dataframes with a new line for each time step, and themselves are outputs of extractUser() 
###  function calls. The last two list elements are matrixes of resource positions in the landscape and yield values in 
###  the landscape (so a single matrix per new time step).
append_output = function(new, existing) {
  
  existing$yield = rbind(existing$yield, t(as.data.frame(extractUser(new, "yield"))))
  existing$budget = rbind(existing$budget, t(as.data.frame(extractUser(new, "budget"))))
  existing$crops = rbind(existing$crops, t(as.data.frame(extractUser(new, "crops"))))
  existing$kills = rbind(existing$kills, t(as.data.frame(extractUser(new, "kills"))))
  existing$scares = rbind(existing$scares, t(as.data.frame(extractUser(new, "scares"))))
  existing$noact = rbind(existing$noact, t(as.data.frame(extractUser(new, "noact"))))
  existing$pop = rbind(existing$pop,  t(as.data.frame(c(new$basic_output$resource_results, 
                                                        new$basic_output$observation_results))))
  existing$res[[ length(existing$res)+1]] = new$RESOURCES
  existing$land_yield[[ length(existing$land_yield) +1]] = new$LAND[,,2]
  
  return(existing)
}