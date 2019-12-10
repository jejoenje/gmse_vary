rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source("helpers.R")
source("gmse_apply_helpers.R")
source("sims/global_paras.R")
source("place_resource.R")

load("sims/nullModel-YTB20/out/20191210074157_nullModel-YTB20/RES_20191210074157.Rdata")
load("sims/nullModel-YTB20/out/20191210074157_nullModel-YTB20/paras_20191210074157_nullModel-YTB20.Rdata")
POP = read.csv("sims/nullModel-YTB20/out/20191210074157_nullModel-YTB20/POP_20191210074157.csv")

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
sim_old$LAND[,,3] = set_land(sim_old$LAND[,,3], s = gmse_paras$stakeholders,
                             type = gmse_paras$land_type, 
                             hi_frac = gmse_paras$land_type_max_frac)

outdir = "pres/pics/anim2" 

sim_example = 100

for(i in 1:len(RES[[sim_example]])) {                    

  pop1 = POP[POP$SIM==sim_example,]
  
  png(file=sprintf("%s/res_dist_pop%03d.png", outdir, i), width=1200, height=600)
    par(mfrow=c(1,2))
    par(oma=c(4,0,0,0))
    par(mar=c(1,1,1,1))
    image(x = sim_old$LAND[,,3], col = brewer.pal(gmse_paras$stakeholders, "BrBG"), xaxt = "n", yaxt = "n");
    res_pos = place_resource(RES[[1]][[i]], ld1 = gmse_paras$land_dim_1, ld2 = gmse_paras$land_dim_2)
    image(x = res_pos, col = "black", add = T)
  
    par(mar=c(4,5,1,1))
    plot(pop1$YEAR, pop1$N, type = "n", ylim = c(0, max(pop1$N, na.rm=T)), cex.lab = 2, cex.axis = 2, 
         ylab = "Population size", xlab = "Year")
    lines(pop1[pop1$YEAR==c(1:(i+1)),"YEAR"],pop1[pop1$YEAR==c(1:(i+1)),"N"], lwd = 3)
    
    mtext(sprintf("Year %d", i), side = 1, line = 2, outer = T, cex = 2)
  dev.off()
  
}

curwd = getwd()
setwd(paste(".", outdir, sep = "/"))
system("convert -delay 30 *.png anim_2.gif")
system("ffmpeg -f gif -i anim_2.gif anim_2.mp4")
setwd(curwd)




