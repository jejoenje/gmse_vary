rm(list=ls())
library(RColorBrewer)

### Load simulation data:
load("sims/gmse_vary_06/sim_summary_file.Rdata")
load("sims/gmse_vary_06/sim_summary.Rdata")

### Find unique "budget combinations":

budgets = unique(para_combs[,7:10])
budgets$budget_type = 1:nrow(budgets)

para_combs = merge(para_combs, budgets)

### Calculate % dev from pop target:
# At t = 40:
para_combs$devTarget40 = ((para_combs[,grep("mnPop", names(para_combs))]-1000)/1000)[,40]
# Over all time steps:
para_combs$mnDevTarget = apply((para_combs[,grep("mnPop", names(para_combs))]-1000)/1000, 1, function(x) mean(x, na.rm=T))

par(mfrow = c(2,3))



### Budget type == 1 (budget equal mb/ub, no effect of yield:)
dat1 = para_combs[para_combs$budget_type == 1, ]

# Stakeholders = 4
cols = brewer.pal(9, "YlOrRd")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(t(dat1[dat1$ov == 0 & dat1$s == 4, grep("mnPop", names(dat1))]), col = cols[1], type = "l", ylim = c(0,2000))
lines(t(dat1[dat1$ov == 0.2 & dat1$s == 4, grep("mnPop", names(dat1))]), col = cols[2])
lines(t(dat1[dat1$ov == 0.4 & dat1$s == 4, grep("mnPop", names(dat1))]), col = cols[3])
lines(t(dat1[dat1$ov == "0.6" & dat1$s == 4, grep("mnPop", names(dat1))]), col = cols[4])
lines(t(dat1[dat1$ov == 0.8 & dat1$s == 4, grep("mnPop", names(dat1))]), col = cols[5])

# Stakeholders = 16
plot(t(dat1[dat1$ov == 0 & dat1$s == 16, grep("mnPop", names(dat1))]), col = cols[1], type = "l", ylim = c(0,2000))
lines(t(dat1[dat1$ov == 0.2 & dat1$s == 16, grep("mnPop", names(dat1))]), col = cols[2])
lines(t(dat1[dat1$ov == 0.4 & dat1$s == 16, grep("mnPop", names(dat1))]), col = cols[3])
lines(t(dat1[dat1$ov == "0.6" & dat1$s == 16, grep("mnPop", names(dat1))]), col = cols[4])
lines(t(dat1[dat1$ov == 0.8 & dat1$s == 16, grep("mnPop", names(dat1))]), col = cols[5])

# Stakeholders = 32
plot(t(dat1[dat1$ov == 0 & dat1$s == 32, grep("mnPop", names(dat1))]), col = cols[1], type = "l", ylim = c(0,2000))
lines(t(dat1[dat1$ov == 0.2 & dat1$s == 32, grep("mnPop", names(dat1))]), col = cols[2])
lines(t(dat1[dat1$ov == 0.4 & dat1$s == 32, grep("mnPop", names(dat1))]), col = cols[3])
lines(t(dat1[dat1$ov == "0.6" & dat1$s == 32, grep("mnPop", names(dat1))]), col = cols[4])
lines(t(dat1[dat1$ov == 0.8 & dat1$s == 32, grep("mnPop", names(dat1))]), col = cols[5])


### Budget type == 2 (budget function of yield)
dat2 = para_combs[para_combs$budget_type == 2, ]

# Stakeholders = 4
cols = brewer.pal(9, "YlOrRd")
cols = tail(cols,5)
cols = cols[order(cols, decreasing = T)]
plot(t(dat2[dat2$ov == 0 & dat2$s == 4, grep("mnPop", names(dat2))]), col = cols[1], type = "l", ylim = c(0,2000))
lines(t(dat2[dat2$ov == 0.2 & dat2$s == 4, grep("mnPop", names(dat2))]), col = cols[2])
lines(t(dat2[dat2$ov == 0.4 & dat2$s == 4, grep("mnPop", names(dat2))]), col = cols[3])
lines(t(dat2[dat2$ov == "0.6" & dat2$s == 4, grep("mnPop", names(dat2))]), col = cols[4])
lines(t(dat2[dat2$ov == 0.8 & dat2$s == 4, grep("mnPop", names(dat2))]), col = cols[5])

# Stakeholders = 16
plot(t(dat2[dat2$ov == 0 & dat2$s == 16, grep("mnPop", names(dat2))]), col = cols[1], type = "l", ylim = c(0,2000))
lines(t(dat2[dat2$ov == 0.2 & dat2$s == 16, grep("mnPop", names(dat2))]), col = cols[2])
lines(t(dat2[dat2$ov == 0.4 & dat2$s == 16, grep("mnPop", names(dat2))]), col = cols[3])
lines(t(dat2[dat2$ov == "0.6" & dat2$s == 16, grep("mnPop", names(dat2))]), col = cols[4])
lines(t(dat2[dat2$ov == 0.8 & dat2$s == 16, grep("mnPop", names(dat2))]), col = cols[5])

# Stakeholders = 32
plot(t(dat2[dat2$ov == 0 & dat2$s == 32, grep("mnPop", names(dat2))]), col = cols[1], type = "l", ylim = c(0,2000))
lines(t(dat2[dat2$ov == 0.2 & dat2$s == 32, grep("mnPop", names(dat2))]), col = cols[2])
lines(t(dat2[dat2$ov == 0.4 & dat2$s == 32, grep("mnPop", names(dat2))]), col = cols[3])
lines(t(dat2[dat2$ov == "0.6" & dat2$s == 32, grep("mnPop", names(dat2))]), col = cols[4])
lines(t(dat2[dat2$ov == 0.8 & dat2$s == 32, grep("mnPop", names(dat2))]), col = cols[5])






