gmse_itr = function(sim_old, yrs = yrs) {

  sim_pop = matrix(data = NA, nrow = yrs, ncol = 3)
  sim_act = list()
  sim_usr = list()
  sim_res = list()

  for(time_step in 1:yrs){
    print(sprintf("time step %d", time_step))
    sim_new <- gmse_apply(get_res = "Full", old_list = sim_old) 
    sim_pop[time_step, 1] = time_step                                         # time step
    sim_pop[time_step, 2] = sim_new$basic_output$resource_results[1]          # Pop size
    sim_pop[time_step, 3] = sim_new$basic_output$observation_results[1]       # Pop est
    sim_act[[time_step]] = sim_new$AGENTS                                     # Full AGENT array for time step: [,16] = yield, [,17] = budget
    sim_usr[[time_step]] = sim_new$basic_output$user_results
    sim_res[[time_step]] = sim_new$RESOURCES
    
    sim_old = sim_new
  }
  
  sim_pop = as.data.frame(sim_pop)
  
  names(sim_pop) = c("Time", "Pop_size", "Pop_est")
  
  return(list(sim_pop, sim_act, sim_usr, sim_res))
  
}
