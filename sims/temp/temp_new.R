rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source("helpers.R")
source("gmse_apply_helpers.R")
rm(set_land)
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

mycols = brewer.pal(9, "BrBG")
mycols = c(mycols[which(mycols=="#F5F5F5")],mycols[which(mycols!="#F5F5F5")])

jpeg("pres/pics/land_example_new.jpg", width = 800, height = 800)

par(mfrow=c(2,2))

source("set_land_new.R")

gmse_paras$land_type = "equal"
gmse_paras$land_type_max_frac = 0
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])
cols = mycols[as.numeric(names(table(sim_old$LAND[,,3])))]
image(x = sim_old$LAND[,,3], col = cols, xaxt = "n", yaxt = "n")

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.25
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])
cols = mycols[as.numeric(names(table(sim_old$LAND[,,3])))]
image(x = sim_old$LAND[,,3], col = cols, xaxt = "n", yaxt = "n")

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.5
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])
cols = mycols[as.numeric(names(table(sim_old$LAND[,,3])))]
image(x = sim_old$LAND[,,3], col = cols, xaxt = "n", yaxt = "n")

gmse_paras$land_type = "oneRich"
gmse_paras$land_type_max_frac = 0.75
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)
table(sim_old$LAND[,,3])
cols = mycols[as.numeric(names(table(sim_old$LAND[,,3])))]
image(x = sim_old$LAND[,,3], col = cols, xaxt = "n", yaxt = "n")

dev.off()
