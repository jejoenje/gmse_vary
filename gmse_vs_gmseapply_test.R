library(GMSE)
rm(list = ls())

stakeholders = 4
land_ownership = TRUE
scaring = TRUE
tend_crops = TRUE
tend_crop_yld = 0.8 # 0.475
agent_view = 10
times_observe = 1
res_move_obs = TRUE
agent_move = 100

K = 50 # 50 years

### Simulations using gmse()
# 
# gmse_sims = gmse(time_max = K, 
#                  stakeholders = stakeholders, 
#                  tend_crops = tend_crops, 
#                  scaring = scaring,
#                  tend_crop_yld = tend_crop_yld, 
#                  land_ownership = land_ownership,
#                  agent_view = agent_view, 
#                  times_observe = times_observe)
# 
# plot_gmse_results(gmse_sims)
# 
# # Number of "tend crop" actions for user no. 2:
# unlist(lapply(gmse_sims$action, function(x) x[,,2][2,10]))


### Repeating same simulations but with "manual" gmse_apply() loop:

sim_old = gmse_apply(get_res = "Full",
                 stakeholders = stakeholders, tend_crops = tend_crops, scaring = scaring,
                 tend_crop_yld = tend_crop_yld, land_ownership = land_ownership,
                 agent_view = agent_view, times_observe = times_observe)
sims = list()
sims[[1]] = sim_old

for(i in 2:K) {
  print(sprintf("Time step %d", i))
  sims[[i]] = gmse_apply(old_list = sims[[i-1]], get_res = "Full")
}

unlist(lapply(sims, function(x) x$ACTION[,,2][2,10]))


