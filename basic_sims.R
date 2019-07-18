rm(list=ls())
library(GMSE)
source('plot_gmse_sims.R')

K <- 93870 
N <- 70000     
target <- 70000 
sc <- 2
time_steps <- 100
sholders <- 8

sim_old <- gmse_apply(get_res = "Full",
                      land_dim_1 = 100,
                      land_dim_2 = 100,
                      land_ownership = TRUE,
                      tend_crops = TRUE,
                      scaring = TRUE,
                      remove_pr = 0.20,          
                      lambda = 0.275,             
                      res_death_K = K/sc,         
                      RESOURCE_ini = N/sc,       
                      manage_target = target/sc,
                      res_death_type = 3,
                      manager_budget = 5000, 
                      user_budget = 5000,
                      public_land = 0.1,            # 0.4
                      stakeholders = sholders, 
                      res_consume = 0.02,
                      res_birth_K = 200000/sc,
                      observe_type = 3, 
                      agent_view = 1, 
                      converge_crit = 0.02,
                      ga_mingen = 100)

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
}
cnames <- c("time", "pop", "pop_est", "cull_cost", "cull_count", "scare_cost","scare_n","tend_n")
names(sims) <- c(cnames, paste0("yield",1:sholders))

plot_gmse_sims(sims)
