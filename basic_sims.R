library(GMSE)

K <- 67980      # K as per goose report, 672+6.655*10114 (latter ha of AIG in 2015)
N <- 42838      # Observed Islay 2015
target <- 28000 
sc <- 1

sim_old <- gmse_apply(get_res = "Full",
                      land_dim_1 = 100,
                      land_dim_2 = 100,
                      land_ownership = TRUE,
                      tend_crops = T,
                      scaring = T,
                      remove_pr = 0.05,          # ?
                      lambda = 0.19,                       
                      res_death_K = K/sc,         
                      RESOURCE_ini = N/sc,       
                      manage_target = target/sc,
                      res_death_type = 3,
                      manager_budget = 1000, 
                      user_budget = 1000,
                      public_land = 0.4, 
                      stakeholders = 8, 
                      res_consume = 0.02,
                      res_birth_K = 200000,        # ?
                      observe_type = 3, 
                      agent_view = 1, 
                      converge_crit = 0.02,
                      ga_mingen = 100);

sim_sum_1 <- matrix(data = NA, nrow = 30, ncol = 5);
for(time_step in 1:30){
  sim_new <- gmse_apply(get_res = "Full", old_list = sim_old)
  print(time_step)
  sim_sum_1[time_step, 1] <- time_step
  sim_sum_1[time_step, 2] <- sim_new$basic_output$resource_results[1]
  sim_sum_1[time_step, 3] <- sim_new$basic_output$observation_results[1]
  sim_sum_1[time_step, 4] <- sim_new$basic_output$manager_results[3]
  sim_sum_1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3])
  sim_old <- sim_new
}
colnames(sim_sum_1) <- c("Time", "Pop_size", "Pop_est", "Cull_cost",
                         "Cull_count")
print(sim_sum_1)
