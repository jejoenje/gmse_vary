rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source('helpers.R')
source('gmse_apply_helpers.R')

# Garbage bucket

placeResources = function(res, xd, yd) {
  land_res = matrix(0, nrow = xd, ncol = yd)
  for(i in 1:nrow(res)) {
    land_res[res[i,5]+1,res[i,6]+1] = land_res[res[i,5]+1,res[i,6]+1]+1
  }
  land_res[land_res==0] = NA
  
  return(land_res)
}

yield_res_rel = function(res_pos, yld) {
  res_pos[is.na(res_pos)] = 0
  no_per_cell = as.numeric(names(table(res_pos)))
  mn_yld = as.vector(NULL)
  for(i in 1:length(no_per_cell)) {
    mn_yld = c(mn_yld, mean(yld[res_pos == no_per_cell[i]]))
  }
  return(data.frame(no_per_cell = no_per_cell, mn_yd = mn_yld))
}

source("build_para_grid.R")


### FOR TESTING
gmse_paras$res_movement = 20

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
                      res_movement = gmse_paras$res_movement)

plot_land(sim_old$LAND[,,3], col = "Greys")
plot_land(sim_old$LAND[,,2], col = "Greens")
par(new = T)
res_pos = placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
plot_land(res_pos, col = "Reds")



land_ownership = rasterToPolygons(raster(t(sim_old$LAND[,,3])), dissolve = T)

test = rasterize(land_ownership, raster(ncol=ncol(sim_old$LAND[,,3]), nrow=nrow(sim_old$LAND[,,3])))
plot(test)


plot(land_ownership)


land_ownership = rasterize(land_ownership)
plot(land_ownership)



res_pos = placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
res_pos = raster(t(res_pos))
par(new = T)
plot(res_pos, col = "red", legend = F)



par(new = T)
image(placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2), col = "red")


out = as.data.frame(NULL)
user_yields = as.data.frame(NULL)
for(i in 1:50) {
  res_positions = placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
  yield_res_rel_i = yield_res_rel(res_pos = res_positions, yld = sim_old$LAND[,,2])
  yield_res_rel_i$sim = i
  out = rbind(out, yield_res_rel_i)
  
  user_yields = rbind(user_yields, extractUser(sim_old, "yield"))
  
  sim_new = sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
  sim_old = sim_new
}

par(mfrow=c(1,2))
plot(out$no_per_cell, out$mn_yd, type = "n", xlim = c(0,5), ylim = c(0,max(out$mn_yd)))
for(i in 1:max(out$sim)) {
  out_i = out[out$sim==i,]
  lines(out_i$no_per_cell,out_i$mn_yd, type = "b", bg="grey", col = "black", pch = 21)
}
plot(1:nrow(user_yields), user_yields[,1], ylim = c(min(user_yields),max(user_yields)), type = "n")
apply(user_yields, 2, function(x) lines(x) )



image(sim_old$LAND[,,2], col = brewer.pal(9, "Greys"))
par(new = T)
res_positions = placeResources(sim_old$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
image(res_positions, col = "red")

sim_new = gmse_apply(get_res = "Full", old_list = sim_old, res_consume = 0.5)

image(sim_new$LAND[,,2], col = brewer.pal(9, "YlGn"))
par(new = T)
res_positions = placeResources(sim_new$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
image(res_positions, col = alpha(brewer.pal(9,"Reds")[6:9], 0.75))
sim_old = sim_new



image(sim_new$LAND[,,2], col = brewer.pal(9, "YlGn"))
par(new = T)
res_positions = placeResources(sim_new$RESOURCES, xd = gmse_paras$land_dim_1, yd = gmse_paras$land_dim_2)
image(res_positions, col = alpha(brewer.pal(9,"Reds")[6:9], 0.75))

sim_old = sim_new



# plot_land(sim_old$LAND[,,3])
# sim_old$LAND[,,3] = distribute_land(xd = gmse_paras$land_dim_1, 
#                                     yd = gmse_paras$land_dim_2, 
#                                     s = gmse_paras$stakeholders, 
#                                     public_land = gmse_paras$public_land,
#                                     type = gmse_paras$land_type,
#                                     rich_frac = gmse_paras$land_type_max_frac)
# plot_land(sim_old$LAND[,,3])

yields = as.data.frame(NULL)
kills = as.data.frame(NULL)
scares = as.data.frame(NULL)
crops = as.data.frame(NULL)

for(i in 1:100) {
  sim_new = gmse_apply(get_res = "Full", old_list=sim_old)
  yields = rbind(yields, extractUser(sim_new, "yield"))
  kills = rbind(kills, extractUser(sim_new, "kills"))
  scares = rbind(scares, extractUser(sim_new, "scares"))
  crops = rbind(crops, extractUser(sim_new, "crops"))
  
  sim_old = sim_new
}
par(mfrow = c(2,2))
plot(yields[,1], ylim = c(min(yields),max(yields)), type = "n")
apply(yields, 2, function(x) lines(x))
plot(kills[,1], ylim = c(min(kills),max(kills)), type = "n")
apply(kills, 2, function(x) lines(x))
plot(scares[,1], ylim = c(min(scares),max(scares)), type = "n")
apply(scares, 2, function(x) lines(x))
plot(crops[,1], ylim = c(min(crops),max(crops)), type = "n")
apply(crops, 2, function(x) lines(x))



hist(sample_budgets_ManyPoorFewRich(nstakeholders = gmse_paras$stakeholders, 
                                    manager_budget = gmse_paras$manager_budget, 
                                    scale = 2), 
     breaks = 8)





res = list()

for(sim in 1:sims) {
  
  res_year = as.list(rep(NA, years))

    
  for(year in 1:years) {

    sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
    if(class(sim_new)=="try-error") {
      if(grepl("Extinction", sim_new[1])) {
        print(sprintf("True extinction, skipping to next sim."))
        res_year[year:years] = "Extinction (true)"
        break()
      } else {
        if(grepl("Error in estimate_abundances", sim_new[1])) {
          print(sprintf("Observed extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (observed)"
          break()
        } else {
          print(sprintf("Observed extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (observed, other error)"
          break()
        }
      }
    } else {
      print(sprintf("Sim %d, year %d", sim, year))
      res_year[[year]] = sim_new
      sim_old <- sim_new
    }
  }
  
  res[[sim]] = res_year
  
}

check_len = lapply(res, function(x) lapply(x, len))


### If not all of these are == years, some extinctions occurred.
unlist(lapply(res, len))


unlist(lapply(res, function(x) lapply(x, class)  ))

checkState = data.frame(sim = rep(1:sims, each=years), 
                        year = rep(1:years,sims), 
                        state = unlist(lapply(res, function(x) lapply(x, class)  )))
checkState

y_lims = c(bufRange(min(extract_gmse(res, "resources"), na.rm=T), end = "lo"),
           bufRange(max(extract_gmse(res, "resources"), na.rm=T), end = "hi"))
 
par(mfrow=c(2,2))
plot_resource(res, type="resources", sumtype = "none", ylim = y_lims)
plot_resource(res, type="observations", sumtype = "none", ylim = y_lims)
plot_actions(res, type = "mean")
plot_yield(res, type = "all")

# 
# get_user_data(res, "observations")





