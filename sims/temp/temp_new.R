rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source("helpers.R")
source("gmse_apply_helpers.R")
source("sims/global_paras.R")

###################################
################################### 
###################################
sim_set_name = "temp"
##########################
##############################  
##################################  
###################################

### Initialise simulations (set output folders and paras etc) 
init_sims(sim_set_name)

### Load parameter grid:    
para_grid = read.csv(paste(gsub("/out/", "", outdir),"/para_grid.csv", sep=""), header=T)

# Load paras from grid:
vals = para_grid[1,]

### Update the previously loaded para list with values from grid:
gmse_paras = update_paras_from_grid(vals, gmse_paras)

### Re-save updated parameter list:
save(gmse_paras, file = para_path)

### Initialise outputs (data frames for POP, EXT and USR data):
init_out(s = sims, y = years, users=gmse_paras[["stakeholders"]])

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
                      ga_mingen = gmse_paras$ga_mingen)

### Revised version of set_land:
set_land = function(land, s, type, hi_frac = NULL, up_frac = NULL, lo_frac = NULL) {
  pub_land_present = FALSE
  tland = table(land)
  
  # If assuming equal land dist, return what was given:
  if(type == "equal" | is.null(type) ) {
    return(land)
  }
  
  if(type == "oneRich") {
    
    if("1" %in% names(tland)) {
      pub_land_present = TRUE
      common_land = tland[1]
      tland = tland[2:len(tland)]
      #names(tland) = as.character(1:len(tland))
    } 
    
    pland = sum(tland)
    if(is.null(up_frac)) up_frac = hi_frac+(hi_frac*0.07)
    if(is.null(lo_frac)) lo_frac = hi_frac-(hi_frac*0.07)
    
    rich_frac = rgbeta(1, mean = hi_frac, var = hi_frac/2000, min = lo_frac, max = up_frac)
    rich_size = floor(pland*rich_frac)
    
    rest = pland - rich_size
    
    others = rest/(s-1)
    others = floor(others)
    
    all = c(rich_size, rep(others, s-1))
    all[1] = all[1]+(pland-sum(all))
    
    if(pub_land_present == TRUE) {
      all = c(common_land, all)
      all_cells = as.vector(NULL)
      for(i in 1:len(all)) {
        all_cells = c(all_cells, rep(i, all[i]))
      }
    } else {
      all_cells = as.vector(NULL)
      for(i in 1:len(all)) {
        all_cells = c(all_cells, rep(i+1, all[i]))
      }  
    }
    
    land_out = matrix(all_cells, nrow = nrow(land), ncol = ncol(land))
    
    return(land_out)
  }
}

gmse_paras$land_type = "equal"
gmse_paras$land_type_max_frac = 0
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
s = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(s,"BrBG"), xaxt = "n", yaxt = "n")

# land = sim_old$LAND[,,3]
# s = gmse_paras$stakeholders
# type = gmse_paras$land_type
# hi_frac = gmse_paras$land_type_max_frac

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.25
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
s = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(s,"BrBG"), xaxt = "n", yaxt = "n")

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.5
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
s = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(s,"BrBG"), xaxt = "n", yaxt = "n")

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.75
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
s = len(names(table(sim_old$LAND[,,3])))
image(x = sim_old$LAND[,,3], col = brewer.pal(s,"BrBG"), xaxt = "n", yaxt = "n")

