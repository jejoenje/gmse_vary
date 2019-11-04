rm(list=ls())
source('global_pars.R')
source('helpers.R')

par(mfrow=c(2,2))

all_out <- file.info(list.files("./out", full.names = T))

## 1. This plots the last sim run using "basic" land and budget parameters - i.e. constant and equal across stakeholders

basic <- all_out[grepl("basic", row.names(all_out)),]
basic <- basic[order(basic$mtime, decreasing=T),]
last_basic <- row.names(basic[1,])
load(last_basic)
plot_gmse_sims(dat, main = "Constant")

## 2. This plots the last simulation using varying budgets; see vnbudget_sims.R for specific budget selection (randomly
## selected across users with a mean equal to user_budget and varying by some scalar). Other parameters, including land
## dist are as above.

vbudget <- all_out[grepl("vbudget", row.names(all_out)),]
vbudget <- vbudget[order(vbudget$mtime, decreasing=T),]
last_vbudget <- row.names(vbudget[1,])
load(last_vbudget)
plot_gmse_sims(dat, main = "Varying budgets")

## 3. Simulation run varying land ownership as per Duthie et al 2019 Appendix 4, but user budgets are the same for all.
## Otherwise same as above.

vland <- all_out[grepl("vland_", row.names(all_out)),]
vland <- vland[order(vland$mtime, decreasing=T),]
last_vland <- row.names(vland[1,])
load(last_vland)
plot_gmse_sims(dat, main = "Varying land ownership")

## 4. Varying both land and budget, with land size and budget positively correlated.

vlandbudget <- all_out[grepl("vlandbudget_", row.names(all_out)),]
vlandbudget <- vlandbudget[order(vlandbudget$mtime, decreasing=T),]
last_vlandbudget <- row.names(vlandbudget[1,])
load(last_vlandbudget)
plot_gmse_sims(dat, main = "Varying land ownership * budget")

tiff(filename="scenarios1.tiff")
dev.copy()
dev.off()





