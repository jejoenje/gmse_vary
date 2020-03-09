### Working revised plotting functions

gmse_vary_plot = function(dat, type = "pop", col = "black", lwd = 1, ylim = NULL, xlim = NULL) {
  
  par(mfrow = c(1,1))
  
  if(type == "pop") {
    # Population trajectories, one for each sim:
    y_lo = min(unlist(lapply(dat, function(x) min(x$pop[,1]))))
    y_hi = max(unlist(lapply(dat, function(x) max(x$pop[,1]))))
    plot(dat[[1]]$pop[,1], type = "n", ylim = c(y_lo,y_hi), xlim = c(0,dat[[1]]$par$n_years)+1)
    lapply(dat, function(x) lines(x$pop[,1], col = col, lwd = lwd))
  }
    
}