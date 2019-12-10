# Plots mean user budgets from a given output index.
# Calculates the mean budget for each user in each year of a given output index.

plot_budgets = function(dfile, xlab = "", ylab = "", cex.axis = 1.5, cex.lab = 1.5, 
                        ylim = NULL, xaxt = "s", yaxt = "s", col = "black", lwd = 2) {
  
  dfolder = as.vector(dfile)
  dfolder = paste("sims",strsplit(as.vector(dfile), "_")[[1]][2],"out",dfile,sep="/")
  dfiles = list.files(dfolder)
  pfile = dfiles[grep("USR", dfiles)]
  p = read.csv(paste(dfolder, pfile, sep="/"),header=T)  

  yr_mn_buds = tapply(p$bud, list(p$usr, p$YEAR), function(x) mean(x, na.rm=T))
  
  if(is.null(ylim)) { 
    ylim = c(min(yr_mn_buds), max(yr_mn_buds))
  }

  plot(1:ncol(yr_mn_buds), yr_mn_buds[1,], type = "n", ylim = ylim, 
       xlab = xlab, ylab = ylab, cex.axis = cex.axis, cex.lab = cex.lab, xaxt = xaxt, yaxt = yaxt, col = col)
  
  for(i in 1:nrow(yr_mn_buds)) {
    lines(1:len(yr_mn_buds[i,]), yr_mn_buds[i,], col = col, lwd = lwd)
  }
  
}

