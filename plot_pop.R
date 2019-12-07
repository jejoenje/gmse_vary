### Plotting individual simulation population trajectories, given output simulation file index.
### Default is to plot all of them.
### Parameter s sets proportion of trends to plot (e.g. 0.5 plots half of all pop trajectories).

plot_pop = function(dfile, col = "Black", ylim = NULL, s = NULL,
                    cex.axis = 1.5, cex.lab = 1.5, xlab = "", ylab = "", yaxt = "s") {
  dfolder = as.vector(dfile)
  dfolder = paste("sims",strsplit(as.vector(dfile), "_")[[1]][2],"out",dfile,sep="/")
  dfiles = list.files(dfolder)
  pfile = dfiles[grep("POP", dfiles)]
  p = read.csv(paste(dfolder, pfile, sep="/"),header=T)
  
  # Plot "time step 0" at expense of last time step:

  p$N[which(is.na(p$N))] = 0
  
  if (is.null(ylim)) { ylim = c(0, max(p$N, na.rm=T)) }
  plot(p$YEAR, p$N, type = "n", ylim = ylim, yaxt = yaxt,
       cex.axis = cex.axis, cex.lab = cex.lab, ylab = ylab, xlab = xlab)
  
  if (is.null(s)) { plotflag = rep(1,max(p$SIM))}
  if (!is.null(s)) { 
    plotflag = sample(1:max(p$SIM), s*100, replace = F)
  }
  for(i in 1:max(p$SIM)) {
    col_i = col
    p_i = p[p$SIM==i, ]
    
    # Plot "time step 0" at expense of last time step:
    p_i = head(p_i, nrow(p_i)-1)
    p_i = rbind(p_i[1,],p_i)
    p_i = rbind(p_i[1,],p_i)
    p_i[1,"N"] = 1000
    p_i$YEAR = 1:len(p_i$YEAR)
    
    if( i %in% plotflag ) {
      if( sum(p_i$N==0)>0 ) { col_i = alpha("red", 0.5) } 
      lines(p_i$YEAR, p_i$N, col = col_i, lwd = 1.5)
    }
  }
}
