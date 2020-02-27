rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source('helpers.R')
source('gmse_apply_helpers.R')

source("build_para_grid.R")

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

image(sim_old$LAND[,,2], col = brewer.pal(9, "Greens"))
par(new = T)
res_pos = placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
image(res_pos, col = "red", xaxt = "n", yaxt = "n")

res_new = sim_old$RESOURCES
res_new[,5:6] = move_resources_adjusted(sim_old$RESOURCES, sim_old$LAND[,,2], buffer = gmse_paras$res_movement)

res_new = placeResources(res_new, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
par(new=T)
image(res_new, col = "blue")

### Further tests with changes to res_move_adjusted()
image(sim_old$LAND[,,2], col = brewer.pal(9, "Greens"))
par(new = T)
res_pos = placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
image(res_pos, col = "red", xaxt = "n", yaxt = "n")

res_new = sim_old$RESOURCES
res_new[,5:6] = move_resources_adjusted(sim_old$RESOURCES, sim_old$LAND[,,2], buffer = gmse_paras$res_movement, type = "max")
res_new = placeResources(res_new, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
par(new=T)
image(res_new, col = "blue")


list(yield = t(as.data.frame(extractUser(sim_old, "yield"))),
     budget = t(as.data.frame(extractUser(sim_old, "budget"))),
     crops =  t(as.data.frame(extractUser(sim_old, "crops"))),
     kills = t(as.data.frame(extractUser(sim_old, "kills"))),
     scares = t(as.data.frame(extractUser(sim_old, "scares"))),
     noact =  t(as.data.frame(extractUser(sim_old, "noact"))),
     land_yield = list(as.matrix(NA)),
     land_res = list(as.matrix(NA))
     )

for(i in 1:10) {
  sim_old$RESOURCES[,5:6] = move_resources_adjusted(sim_old$RESOURCES, sim_old$LAND[,,2], 
                                                    buffer = gmse_paras$res_movement, 
                                                    type = "max")
  
  sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
}
