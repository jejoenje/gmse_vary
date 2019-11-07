

gmse_paras <- list(
  get_res = "Full",
  land_dim_1 = 200,       
  land_dim_2 = 200,
  land_ownership = TRUE, 
  tend_crops = TRUE,
  tend_crop_yld = 0.2,
  scaring = TRUE,
  remove_pr = 0.45,           
  res_death_type = 3,
  lambda = 0.3,
  res_death_K = 10000,         
  RESOURCE_ini = 4000,       
  manage_target = 1000,
  manager_budget = 2000, 
  user_budget = 2000,
  public_land = 0,    
  stakeholders = 5, 
  res_consume = 0.5,     
  observe_type = 0,
  res_move_obs = TRUE,
  agent_view = 100,           # Keep to land_dim_1/5 ?
  times_observe = 1,
  agent_move = 100,          # Keep to land_dim_1/2 ? (as default?)
  converge_crit = 0.1,
  ga_mingen = 40)
