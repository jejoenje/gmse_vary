### Scenario 1 output plotting

rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
source('helpers.R')
source('gmse_apply_helpers.R')

sims = gmse_rds_summary("sims/scenario1/")

### Find and count extinctions:

unlist(lapply(sims, function(x) nrow(x$pop)))


# # For plotting output above
par(mfrow=c(1,2))
# Population trajectories, one for each sim:
y_lo = min(unlist(lapply(sims, function(x) min(x$pop[,1]))))
y_hi = max(unlist(lapply(sims, function(x) max(x$pop[,1]))))
plot(sims[[1]]$pop[,1], type = "n", ylim = c(y_lo,y_hi), xlim = c(0,sims[[1]]$par$n_years)+1)
lapply(sims, function(x) lines(x$pop[,1], col = "darkred"))
# Mean yields across users, for each sim:
mean_yields = lapply(sims, function(x) apply(x$yield, 1, mean))
y_lo = min(unlist(lapply(mean_yields, min)))
y_hi = max(unlist(lapply(mean_yields, max)))
plot(mean_yields[[1]], type = "n", ylim = c(y_lo,y_hi), xlim = c(0,sims[[1]]$par$n_years)+1)
lapply(mean_yields, function(x) lines(x, col = "darkgreen"))
