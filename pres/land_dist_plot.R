rm(list=ls())
library(GMSE)
library(RColorBrewer)
library(scales)
source("../sims/global_paras.R")
source("../gmse_apply_helpers.R")
source("../helpers.R")

PUB_LAND = 0

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
                      public_land = PUB_LAND,
                      stakeholders = gmse_paras$stakeholders, 
                      res_consume = gmse_paras$res_consume,  
                      observe_type = gmse_paras$observe_type, 
                      agent_view = gmse_paras$agent_view,
                      agent_move = gmse_paras$agent_move,
                      converge_crit = gmse_paras$converge_crit,
                      ga_mingen = gmse_paras$ga_mingen)

jpeg("pics/land_dist_examples.jpg", quality = 100, height = 800, width = 800)

par(mfrow=c(2,2))
par(mar = c(2,2,4,2))

cex.labels = 3.5

gmse_paras$land_type = "equal"
gmse_paras$land_type_max_frac = NA
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type,
                             hi_frac = gmse_paras$land_type_max_frac)

table(sim_old$LAND[,,3])
N = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(N, "BrBG"), xaxt = "n", yaxt = "n");
mtext("Equal", side = 3, line = 0.5, cex = cex.labels)

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.25
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])
N = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(N, "BrBG"), xaxt = "n", yaxt = "n");
mtext("25%", side = 3, line = 0.5, cex = cex.labels)


gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.5
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])[2:9]
N = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(N, "BrBG"), xaxt = "n", yaxt = "n");
mtext("50%", side = 3, line = 0.5, cex = cex.labels)

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.75
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])
N = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(N, "BrBG"), xaxt = "n", yaxt = "n");
mtext("75%", side = 3, line = 0.5, cex = cex.labels)

dev.off()
