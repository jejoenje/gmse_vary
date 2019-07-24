rm(list=ls())
library(GMSE)
source('helpers.R')
source('global_pars.R')

z <- 4 # 4 repeat sims

for(i in 1:z) {

  # This draws 8 random budgets with a mean of u_budget and a standard dev of u_budget/u_budget_scale.
  # The idea is that this budget will stay the same for each stakeholder throughout all time steps.
  ubudgets <- floor(rnorm(sholders, u_budget, u_budget/u_budget_vscale))
  
  #ubudgets <- c(250,250,250,900,900,900,900,1200)
    
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
  
  sim_old$AGENTS[sim_old$AGENTS[,2]==1,17] <- ubudgets
  
  sims <- as.data.frame(NULL)
  for(t in 1:time_steps){
    
    sim_new <- gmse_apply(get_res = "Full", old_list = sim_old)
    print(t)
    
    sim_t <- c(t, sim_new$basic_output$resource_results[1],
               sim_new$basic_output$observation_results[1],
               sim_new$basic_output$manager_results[3],
               sum(sim_new$basic_output$user_results[,3]),
               sim_new$basic_output$manager_results[2],
               sum(sim_new$basic_output$user_results[,2]),
               sum(sim_new$basic_output$user_results[,7]))
    sim_t <- c(sim_t, round(sim_new$AGENTS[2:nrow(sim_new$AGENTS),16],3))
    
    sims <- rbind(sims, sim_t)
    sim_old <- sim_new
    sim_old$AGENTS[,16] <- 0

  }
  cnames <- c("time", "pop", "pop_est", "cull_cost", "cull_count", "scare_cost","scare_n","tend_n")
  names(sims) <- c(cnames, paste0("yield",1:sholders))
  
  saved(sims, "vbudget")
}

mtimes <- file.info(list.files("./out", full.names = T))
mtimes <- mtimes[order(mtimes$mtime, decreasing=T),]
mtimes <- mtimes[grepl("vbudget", row.names(mtimes)),]
recent_files <- row.names(mtimes[1:z,])

par(mfrow=c(2,2))
for(i in 1:z) {
  load(recent_files[i])
  plot_gmse_sims(dat)
}

### Check yield calcs for last iteration:
yields <- as.vector(NULL)
for (i in 1:sholders) {
  yields <- c(yields, sum(sim_new$LAND[,,2][which(sim_new$LAND[,,3]==i+1)]))
}
round(yields,2)
# Should be the same as
round(tail(sims[,grepl("yield", names(sims))],1),2)

