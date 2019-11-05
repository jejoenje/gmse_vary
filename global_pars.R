K <- 10000                # 15000
N <- 2800                 # 5000 # 4850
target <- 2800            # 5000
sc <- 1
time_steps <- 100
sholders <- 10             # 8
birth_K = 10000           # 15000
u_budget <- 1000
u_budget_vscale <- 8

gmse_paras <- list(
  get_res = "Full",
  land_dim_1 = 300,       # 127x127 Results in equivalent ~ 16129 ha of potentially suitable habitat
  land_dim_2 = 300,
  land_ownership = TRUE,
  tend_crops = TRUE,
  tend_crop_yld = 0.3,
  scaring = TRUE,
  remove_pr = 0.15,   # 0.20         
  lambda = 0.275, # 0.275            
  res_death_K = K,         
  RESOURCE_ini = N,       
  manage_target = target,
  res_death_type = 3,
  manager_budget = 3000,  # 1000
  user_budget = u_budget,
  public_land = 0.1,     # 0.65 results in ~ 564.5 ha per farm given 10 s'holders and 127x127 landscape dims
  stakeholders = sholders, 
  res_consume = 0.2,     # 0.035   # 0.1   # 0.3
  res_birth_K = birth_K,
  observe_type = 0,
  res_move_obs = TRUE,
  agent_view = 20,
  times_observe = 1,
  agent_move = 100,
  converge_crit = 0.02,
  ga_mingen = 100)
