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
par(oma = c(6,2,0,0))
# vmar = c(rep(c(0.5,2,0.5,1),9),     # Top 3 rows
#          rep(c(0.5,2,0.5,1),3))     # Bottom row
# vmar = matrix(vmar, nrow = 12, ncol = 4, byrow = T)
vxaxt = c(rep("n", 9),rep("n",3))
par(mar = c(0.5,4,0.5,4))

paras = expand.grid(OV = OV, S = S)

for(i in 1:nrow(paras)) {
  pick = para_combs[para_combs$uyb==1 & para_combs$s == paras$S[i] & para_combs$ov == paras$OV[i],]
  mnpop = t(pick[,grep("mnPop", names(pick))])
  sdpop = t(pick[,grep("sdPop", names(pick))])
  mnyld = t(pick[,grep("mnYld", names(pick))])
  sdyld = t(pick[,grep("sdYld", names(pick))])
  pop_lo = mnpop-sdpop
  pop_lo[pop_lo<0] = 0
  pop_lo = pop_lo[!is.na(pop_lo)]
  pop_hi = mnpop+sdpop
  pop_hi = pop_hi[!is.na(pop_hi)]
  yld_lo = mnyld-sdyld
  yld_lo[yld_lo<0] = 0
  yld_lo = yld_lo[!is.na(yld_lo)]
  yld_hi = mnyld+sdyld
  yld_hi = yld_hi[!is.na(yld_hi)]
  
  #par(mar=vmar[i,])
  plot(mnpop, ylim = c(0,1600), type = "n", xaxt = vxaxt[i])
  text(x = 20, y = 1500, paste("s = ",paras$S[i], ", ov = ",paras$OV[i],sep = ""), cex = 1.25)
  polygon(c(1:length(pop_lo),rev(1:length(pop_lo))),c(pop_lo,rev(pop_hi)),col=alpha("skyblue",0.5),border=NA)
  lines(mnpop, lwd = 2)
  par(new = TRUE)
  plot(mnyld, type = "l", axes = F, col = "darkgreen", lwd = 2, ylab = "", ylim = c(min(yld_lo)*0.9,max(yld_hi)*1.1))
  polygon(c(1:length(yld_lo),rev(1:length(yld_lo))),c(yld_lo,rev(yld_hi)),col=alpha("lightgreen",0.5),border=NA)
  axis(side = 4, at = pretty(range(mnyld)), labels = FALSE)
  if(i<10) {
    axis(side = 1, at = pretty(range(1:40)), labels = FALSE)
  }
  if(i>9) {
    axis(side = 1, at = pretty(range(1:40)), xpd = NA)
  }
}



# lines(pop_lo)
# lines(pop_hi)
