rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source('helpers.R')
source('gmse_apply_helpers.R')

source("build_para_grid.R")

n_sims = gmse_paras$n_sims
n_years = gmse_paras$n_years

sims = list()

for(K in 1:n_sims) {
  
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
                        ga_mingen = gmse_paras$ga_mingen,
                        res_movement = gmse_paras$res_movement,
                        res_move_type = gmse_paras$res_move_type)
  
  out_i = list(yield = t(as.data.frame(extractUser(sim_old, "yield"))),
               budget = t(as.data.frame(extractUser(sim_old, "budget"))),
               crops =  t(as.data.frame(extractUser(sim_old, "crops"))),
               kills = t(as.data.frame(extractUser(sim_old, "kills"))),
               scares = t(as.data.frame(extractUser(sim_old, "scares"))),
               noact =  t(as.data.frame(extractUser(sim_old, "noact"))),
               pop = t(as.data.frame(c(sim_old$basic_output$resource_results, 
                                       sim_old$basic_output$observation_results))),
               res = list(sim_old$RESOURCES),
               land_yield = list(sim_old$LAND[,,2])
  )
  
  for(i in 1:n_years) {
    #print(paste("Time step", i, "..."))
    print(sprintf("Simulation %d, Time step %d", K, i))
    
    ### Move resources according to yield, if needed:
    if(gmse_paras$res_move_to_yield==TRUE && gmse_paras$res_move_type==0 ) {
      sim_old$RESOURCES[,5:6] = move_resources_adjusted(sim_old$RESOURCES, sim_old$LAND[,,2], 
                                                        buffer = gmse_paras$res_movement, 
                                                        type = "max")  
    }
    if(gmse_paras$res_move_to_yield==TRUE && gmse_paras$res_move_type!=0) {
      stop("Invalid res_move_to_yield / res_move_type combination")
    }
    if(gmse_paras$res_move_to_yield==FALSE && gmse_paras$res_move_type==0) {
      stop("res_move_type set to 0 (no movement) but res_move_to_yield also FALSE - static resources!")
    }
    
    ### Try to run next time step
    sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
    
    ### Check output of next time step; if there are errors (extinctions or no obs), skip to the next sim.
    if(class(sim_new)=="try-error") {
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
      
      ### If there are no errors in the next time step, store key results:  
      out_i$yield = rbind(out_i$yield, t(as.data.frame(extractUser(sim_new, "yield"))))
      out_i$budget = rbind(out_i$budget, t(as.data.frame(extractUser(sim_new, "budget"))))
      out_i$crops = rbind(out_i$crops, t(as.data.frame(extractUser(sim_new, "crops"))))
      out_i$kills = rbind(out_i$kills, t(as.data.frame(extractUser(sim_new, "kills"))))
      out_i$scares = rbind(out_i$scares, t(as.data.frame(extractUser(sim_new, "scares"))))
      out_i$noact = rbind(out_i$noact, t(as.data.frame(extractUser(sim_new, "noact"))))
      out_i$pop = rbind(out_i$pop,  t(as.data.frame(c(sim_new$basic_output$resource_results, 
                                                      sim_new$basic_output$observation_results))))
      out_i$res[[i+1]] = sim_new$RESOURCES
      out_i$land_yield[[i+1]] = sim_old$LAND[,,2]
      
      ### ... reset current time step data, and remove the existing "new":
      sim_old = sim_new
      rm(sim_new)    
    } # endif error check for next time step 
    
  } # end of year
  
  ### When all years within sims are done, store results for current simulation in next list element
  sims[[K]] = out_i
  
} # end of sims


# For plotting output above
#par(mfrow=c(2,2))
# Population trajectories, one for each sim:
y_lo = min(unlist(lapply(sims, function(x) min(x$pop[,1]))))
y_hi = max(unlist(lapply(sims, function(x) max(x$pop[,1]))))
plot(sims[[1]]$pop[,1], type = "n", ylim = c(y_lo,y_hi))
lapply(sims, function(x) lines(x$pop[,1], col = "darkred"))
# Mean yields across users, for each sim:
mean_yields = lapply(sims, function(x) apply(x$yield, 1, mean))
y_lo = min(unlist(lapply(mean_yields, min)))
y_hi = max(unlist(lapply(mean_yields, max)))
plot(mean_yields[[1]], type = "n", ylim = c(y_lo,y_hi))
lapply(mean_yields, function(x) lines(x, col = "darkgreen"))
