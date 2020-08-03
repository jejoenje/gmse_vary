rm(list=ls())

### Collate data from different batches:

# Batch 1:
load("sims/gmse_vary_06/batch1_summary.Rdata")
para_combs1 = para_combs
# Batch 2:
load("sims/gmse_vary_06/batch2_summary.Rdata")
para_combs2 = para_combs
rm(para_combs)

# Combine separate batch summaries:
para_combs = rbind(para_combs1, para_combs2)

### Calculate % dev from pop target:
# At t = 40:
para_combs$devTarget40 = ((para_combs[,grep("mnPop", names(para_combs))]-1000)/1000)[,40]
# Over all time steps:
para_combs$mnDevTarget = apply((para_combs[,grep("mnPop", names(para_combs))]-1000)/1000, 1, function(x) mean(x, na.rm=T))


# Exploring different subsets of results - 

# For single number of stakeholders (32):
para_combs[para_combs$s==32,1:14]
# Single no. stakeholders and ub == mb:
para_combs[para_combs$s==32 & para_combs$ub==para_combs$mb,1:14]
# Single no. stakeholders, ub == mb and one value for uyb/myb:
para_combs[para_combs$s==32 & para_combs$ub==para_combs$mb & para_combs$uyb==0,1:14]
para_combs[para_combs$s==32 & para_combs$ub==para_combs$mb & para_combs$uyb==1,1:14]
para_combs[para_combs$s==32 & para_combs$ub==para_combs$mb & para_combs$uyb==10,1:14]
para_combs[para_combs$s==32 & para_combs$ub==para_combs$mb & para_combs$uyb==100,1:14]


par(mfrow=c(2,2))
d1 = para_combs[para_combs$s==24 & para_combs$ub==para_combs$mb & para_combs$uyb==0,1:14]
d2 = para_combs[para_combs$s==24 & para_combs$ub==para_combs$mb & para_combs$uyb==1,1:14]
d3 = para_combs[para_combs$s==24 & para_combs$ub==para_combs$mb & para_combs$uyb==10,1:14]
d4 = para_combs[para_combs$s==20 & para_combs$ub==para_combs$mb & para_combs$uyb==100,1:14]

barplot(ext ~ ov, data = d1, ylim = c(0,100))
barplot(ext ~ ov, data = d2, ylim = c(0,100))
barplot(ext ~ ov, data = d3, ylim = c(0,100))
barplot(ext ~ ov, data = d4, ylim = c(0,100))




### Plot for a single par_set:
par(mfrow = c(1,3))
par(mar = c(1,2,1,1))
par(oma = c(5,16,3,16))

### Budget type == 7 (budget equal mb/ub, yield-to-budget == 10:)
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
     xlim = c(2, 34), ylim = c(-1,1))
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

mtext("No. of stakeholders", side = 1, outer = TRUE, line = 2.5, cex = 1.5)

mtext("% population extinction", side = 3, outer = TRUE, line = 0.5, adj = 0.07, cex = 1.1)
mtext("Mean years to extinction", side = 3, outer = TRUE, line = 0.5, adj = 0.5, cex = 1.1)
mtext("Mean % deviation from target", side = 3, outer = TRUE, line = 0.5, adj = 0.98, cex = 1.1)

# mtext("Constant budget", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.95)
# mtext("Budget ~ yield", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.7)
# mtext("Constant budget,\nuser < manager budget", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.45)
# mtext("Constant budget,\nuser > manager budget", side = 2, outer = TRUE, line = 1, cex = 1, las = 1, at = 0.2)

