### Plotting simulations
rm(list=ls())
library(scales)  # for alpha() 
source("helpers.R")
source("gmse_vary_helpers.R")

### Load simulation data:
load("sims/gmse_vary_06/sim_summary_file.Rdata")
load("sims/gmse_vary_06/sim_summary.Rdata")

S = c(8,16,24,32)
OV = c(0,0.4,0.8)

par(mfrow = c(4,3))
par(oma = c(4,4,4,4))
vxaxt = c(rep("n", 9),rep("n",3))
par(mar = c(0.75,0.75,0.75,0.75))

paras = expand.grid(OV = OV, S = S)

for(i in 1:nrow(paras)) {
  
  pick = para_combs[para_combs$uyb==0 & para_combs$s == paras$S[i] & para_combs$ov == paras$OV[i],]
  
  p_acts = matrix(NA, nrow = 3, ncol = 40)
  p_acts[1,] = t(pick[,grep("mpKills", names(pick))])
  p_acts[2,] = t(pick[,grep("mpScares", names(pick))])
  p_acts[3,] = t(pick[,grep("mpCrops", names(pick))])
  barplot(p_acts, col = c("#b30000","blue","#41ab5d"), border = c("#b30000","blue","#41ab5d"), space = 0, yaxt = "n")
  
  if(i %in% c(1,4,7,10)) {
    axis(2, at = pretty(range(0,1)), labels = pretty(range(0,1)))
  } else {
    axis(2, at = pretty(range(0,1)), labels = NA)
  }
  
  if(i %in% c(10,11,12)) {
    axis(1, at = pretty(range(1,40)), labels = pretty(range(1,40)))
  } else {
    axis(1, at = pretty(range(1,40)), labels = NA)
  }
  
}
mtext("Years", side = 1, line = 2, outer = T, cex = 1.25)
mtext("Proportion of actions", side = 2, line = 2, outer = T, cex = 1.25)
mtext("ov = 0", side = 3, line = 1, outer = T, cex = 1.25, adj = 0.15)
mtext("ov = 0.4", side = 3, line = 1, outer = T, cex = 1.25, adj = 0.5)
mtext("ov = 0.8", side = 3, line = 1, outer = T, cex = 1.25, adj = 0.85)



# lines(pop_lo)
# lines(pop_hi)
