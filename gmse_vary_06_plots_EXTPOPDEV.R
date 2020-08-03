rm(list=ls())
library(RColorBrewer)

### Load simulation data:
load("sims/gmse_vary_06/batch1_summary_allfiles.Rdata")
load("sims/gmse_vary_06/batch1_summary.Rdata")

### unique "budget combinations":
para_combs[para_combs$par_set==1,c("ub","mb","uyb","myb","ov","s")]  # par_set 1
para_combs[para_combs$par_set==2,c("ub","mb","uyb","myb","ov","s")]  # par_set 2
para_combs[para_combs$par_set==3,c("ub","mb","uyb","myb","ov","s")]  # par_set 3
para_combs[para_combs$par_set==4,c("ub","mb","uyb","myb","ov","s")]  # par_set 4
para_combs[para_combs$par_set==5,c("ub","mb","uyb","myb","ov","s")]  # par_set 5
para_combs[para_combs$par_set==6,c("ub","mb","uyb","myb","ov","s")]  # par_set 6

### Calculate % dev from pop target:
# At t = 40:
para_combs$devTarget40 = ((para_combs[,grep("mnPop", names(para_combs))]-1000)/1000)[,40]
# Over all time steps:
para_combs$mnDevTarget = apply((para_combs[,grep("mnPop", names(para_combs))]-1000)/1000, 1, function(x) mean(x, na.rm=T))

par(mfrow = c(4,3))
par(mar = c(1,2,1,1))
par(oma = c(5,16,3,16))

### Budget type == 1 (budget equal mb/ub, no effect of yield:)
dat1 = para_combs[para_combs$par_set == 1, ]

cols = brewer.pal(9, "YlOrRd")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
cols1 = cols
plot(dat1[dat1$ov==0,"s"], dat1[dat1$ov==0,"ext"], 
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5, 
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,110))
lines(dat1[dat1$ov==0.2,"s"], dat1[dat1$ov==0.2,"ext"], 
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov==0.4,"s"], dat1[dat1$ov==0.4,"ext"], 
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov=="0.6","s"], dat1[dat1$ov=="0.6","ext"], 
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov==0.8,"s"], dat1[dat1$ov==0.8,"ext"], 
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat1$s), labels = NA)

cols = brewer.pal(9, "YlGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat1[dat1$ov==0,"s"], dat1[dat1$ov==0,"mn_n_yrs"], 
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5, 
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,45))
lines(dat1[dat1$ov==0.2,"s"], dat1[dat1$ov==0.2,"mn_n_yrs"], 
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov==0.4,"s"], dat1[dat1$ov==0.4,"mn_n_yrs"], 
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov=="0.6","s"], dat1[dat1$ov=="0.6","mn_n_yrs"], 
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov==0.8,"s"], dat1[dat1$ov==0.8,"mn_n_yrs"], 
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat1$s), labels = NA)

cols = brewer.pal(9, "PuBuGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat1[dat1$ov==0,"s"], dat1[dat1$ov==0,"devTarget40"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "",
     xlim = c(2, 34), ylim = c(-1,0.3))
lines(dat1[dat1$ov==0.2,"s"], dat1[dat1$ov==0.2,"devTarget40"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov==0.4,"s"], dat1[dat1$ov==0.4,"devTarget40"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov=="0.6","s"], dat1[dat1$ov=="0.6","devTarget40"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat1[dat1$ov==0.8,"s"], dat1[dat1$ov==0.8,"devTarget40"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat1$s), labels = NA)
abline(h = 0, lty = "dashed", col = "red")

# ### Budget type == 2 (budget equal mb/ub, budget function of yield)
dat2 = para_combs[para_combs$par_set == 2, c(1:15, ncol(para_combs))]

cols = brewer.pal(9, "YlOrRd")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat2[dat2$ov==0,"s"], dat2[dat2$ov==0,"ext"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,110))
lines(dat2[dat2$ov==0.2,"s"], dat2[dat2$ov==0.2,"ext"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov==0.4,"s"], dat2[dat2$ov==0.4,"ext"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov=="0.6","s"], dat2[dat2$ov=="0.6","ext"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov==0.8,"s"], dat2[dat2$ov==0.8,"ext"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat2$s), labels = NA)

cols = brewer.pal(9, "YlGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat2[dat2$ov==0,"s"], dat2[dat2$ov==0,"mn_n_yrs"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,45))
lines(dat2[dat2$ov==0.2,"s"], dat2[dat2$ov==0.2,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov==0.4,"s"], dat2[dat2$ov==0.4,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov=="0.6","s"], dat2[dat2$ov=="0.6","mn_n_yrs"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov==0.8,"s"], dat2[dat2$ov==0.8,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat2$s), labels = NA)

cols = brewer.pal(9, "PuBuGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat2[dat2$ov==0,"s"], dat2[dat2$ov==0,"devTarget40"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "",
     xlim = c(2, 34), ylim = c(-1,0.3))
lines(dat2[dat2$ov==0.2,"s"], dat2[dat2$ov==0.2,"devTarget40"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov==0.4,"s"], dat2[dat2$ov==0.4,"devTarget40"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat2[dat2$ov=="0.6","s"], dat2[dat2$ov=="0.6","devTarget40"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat2[dat1$ov==0.8,"s"], dat2[dat2$ov==0.8,"devTarget40"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat2$s), labels = NA)
abline(h = 0, lty = "dashed", col = "red")


# ### Budget type == 5 (ub < mb, budget function of yield)
dat3 = para_combs[para_combs$par_set == 3,]

cols = brewer.pal(9, "YlOrRd")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat3[dat3$ov==0,"s"], dat3[dat3$ov==0,"ext"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,110))
lines(dat3[dat3$ov==0.2,"s"], dat3[dat3$ov==0.2,"ext"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov==0.4,"s"], dat3[dat3$ov==0.4,"ext"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov=="0.6","s"], dat3[dat3$ov=="0.6","ext"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov==0.8,"s"], dat3[dat3$ov==0.8,"ext"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat3$s), labels = NA)

cols = brewer.pal(9, "YlGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat3[dat3$ov==0,"s"], dat3[dat3$ov==0,"mn_n_yrs"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,45))
lines(dat3[dat3$ov==0.2,"s"], dat3[dat3$ov==0.2,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov==0.4,"s"], dat3[dat3$ov==0.4,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov=="0.6","s"], dat3[dat3$ov=="0.6","mn_n_yrs"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov==0.8,"s"], dat3[dat3$ov==0.8,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat3$s), labels = NA)

cols = brewer.pal(9, "PuBuGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat3[dat3$ov==0,"s"], dat3[dat3$ov==0,"devTarget40"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "",
     xlim = c(2, 34), ylim = c(-1,5))
lines(dat3[dat3$ov==0.2,"s"], dat3[dat3$ov==0.2,"devTarget40"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov==0.4,"s"], dat3[dat3$ov==0.4,"devTarget40"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat3[dat3$ov=="0.6","s"], dat3[dat3$ov=="0.6","devTarget40"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat3[dat1$ov==0.8,"s"], dat3[dat3$ov==0.8,"devTarget40"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat3$s), labels = NA)
abline(h = 0, lty = "dashed", col = "red")


# ### Budget type == 3 (ub < mb, budget function of yield)
dat4 = para_combs[para_combs$par_set == 4,]

cols = brewer.pal(9, "YlOrRd")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat4[dat4$ov==0,"s"], dat4[dat4$ov==0,"ext"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,110))
lines(dat4[dat4$ov==0.2,"s"], dat4[dat4$ov==0.2,"ext"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov==0.4,"s"], dat4[dat4$ov==0.4,"ext"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov=="0.6","s"], dat4[dat4$ov=="0.6","ext"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov==0.8,"s"], dat4[dat4$ov==0.8,"ext"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat4$s), labels = unique(dat4$s))

cols = brewer.pal(9, "YlGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
cols2 = cols
plot(dat4[dat4$ov==0,"s"], dat4[dat4$ov==0,"mn_n_yrs"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "", xlim = c(2, 34), ylim = c(0,45))
lines(dat4[dat4$ov==0.2,"s"], dat4[dat4$ov==0.2,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov==0.4,"s"], dat4[dat4$ov==0.4,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov=="0.6","s"], dat4[dat4$ov=="0.6","mn_n_yrs"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov==0.8,"s"], dat4[dat4$ov==0.8,"mn_n_yrs"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat4$s), labels = unique(dat4$s))

cols = brewer.pal(9, "PuBuGn")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(dat4[dat4$ov==0,"s"], dat4[dat4$ov==0,"devTarget40"],
     type = "b", pch = 21, col = cols[1], bg = cols[1], lwd = 1.5, cex = 1.5,
     xaxt = "n", xlab = "", ylab = "",
     xlim = c(2, 34), ylim = c(-1,0.3))
lines(dat4[dat4$ov==0.2,"s"], dat4[dat4$ov==0.2,"devTarget40"],
      type = "b", pch = 21, col = cols[2], bg = cols[2], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov==0.4,"s"], dat4[dat4$ov==0.4,"devTarget40"],
      type = "b", pch = 21, col = cols[3], bg = cols[3], lwd = 1.5, cex = 1.5)
lines(dat4[dat4$ov=="0.6","s"], dat4[dat4$ov=="0.6","devTarget40"],
      type = "b", pch = 21, col = cols[4], bg = cols[4], lwd = 1.5, cex = 1.5)
lines(dat4[dat1$ov==0.8,"s"], dat4[dat4$ov==0.8,"devTarget40"],
      type = "b", pch = 21, col = cols[5], bg = cols[5], lwd = 1.5, cex = 1.5)
axis(1, at = unique(dat4$s), labels = unique(dat4$s))
abline(h = 0, lty = "dashed", col = "red")

mtext("No. of stakeholders", side = 1, outer = TRUE, line = 2.5, cex = 1.5)

mtext("% population extinction", side = 3, outer = TRUE, line = 0.5, adj = 0.07, cex = 1.1)
mtext("Mean years to extinction", side = 3, outer = TRUE, line = 0.5, adj = 0.5, cex = 1.1)
mtext("Mean % deviation from target", side = 3, outer = TRUE, line = 0.5, adj = 0.98, cex = 1.1)

mtext("Constant budget", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.95)
mtext("Budget ~ yield", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.7)
mtext("Constant budget,\nuser < manager budget", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.45)
mtext("Constant budget,\nuser > manager budget", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.2)

legend("topright", inset = c(-0.65, -1.75), fill = cols1, legend = c("","","","",""),
       title = "\n", xpd = NA, cex = 2, bty = "n")
legend("topright", inset = c(-0.7, -1.75), fill = cols2, legend = c("","","","",""),
       title = "\n", xpd = NA, cex = 2, bty = "n")
legend("topright", inset = c(-1.5, -1.75), legend = unique(dat4$ov), fill = cols,
       title = "Land ownership\nvariation", xpd = NA, cex = 2, bty = "n", x.intersp	= 0.5)
