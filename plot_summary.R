# Plots simulation summaries; 

plot_summary = function(dat, y = "mean_trend", 
                        tcy = NULL, pl = NULL, mbt = NULL, yv = NULL, col = "RdBu", ylim = NULL, ylab = "", xlab = "",
                        cex.lab = 1.5, cex.axis = 1.5) {
  
  if(is.null(dat)) stop("Missing data!")
  
  paras_names = c("tcy", "pl", "mbt", "yv")
  check_paras = c(is.null(tcy), is.null(pl), is.null(mbt), is.null(yv))
  if(sum(check_paras)>0) {
    missing = paste(paras_names[check_paras],collapse = ", ")
    stop(paste("Missing input parameters:", missing))
  }
  
  d = dat[dat$tend_crop_yld==tcy & dat$yield_value==yv & dat$public_land==pl & dat$man_bud_type==mbt,]
  
  colrange = brewer.pal(9, col)
  colrange = colrange[order(1:len(colrange),decreasing=T)]
  colrange = c(colrange[1], tail(colrange, 3))
  
  par(mar=c(4.5,4.5,1.5,0.5))
  barlabs = c("Equal", d$land_type_max_frac[d$land_type!="equal"])
  
  if(y == "ext_perc") yval = d$EXT
  if(y == "mean_trend") yval = d$trend_Mean
  
  if(y == "trends") {
    dfiles = as.vector(d$idx)
    # THIS PLOTS ALL LINES
    
    pop_dat = list()
    for(i in 1:len(dfiles)) {
      if(!is.na(dfiles[i])) {
        dfolder = unlist(strsplit(dfiles[i],"_"))[2]
        dfolder = paste("sims",dfolder,"out",dfiles[i],sep="/")
        outfiles = list.files(dfolder)
        pop_file = outfiles[grep("POP", outfiles)]
        i_dat = read.csv(paste(dfolder,pop_file,sep="/"), header=T)
        pop_dat[[i]] = i_dat  
      } else {
        pop_dat[[i]] = NULL
      }
      
    }
    lo = suppressWarnings({min(unlist(lapply(pop_dat, function(x) min(x, na.rm=T) )))})
    hi = suppressWarnings({max(unlist(lapply(pop_dat, function(x) max(x, na.rm=T) )))})
    ylims = c(lo,hi)
    
    yrs = max(unlist(lapply(pop_dat, function(x) { if(!is.null(x)) max(x$YEAR) })))    
    plot(1:yrs, rep(1, yrs), ylim = ylims, type = "n")
    
    for(i in 1:nrow(d)) {
      i_dat = pop_dat[[i]]
      if(!is.null(i_dat)) {
        for(j in 1:nlevels(factor(i_dat$SIM))) {
          i_dat_j = i_dat[i_dat$SIM == j,]
          lines(i_dat_j$YEAR, i_dat_j$N, col = alpha(colrange[i], 0.5))
        }  
      }
    }
    
    # ### Attempt at summarising trends:
    # #
    # dfiles = as.vector(d$idx)
    # pop_mns = matrix(NA, ncol = nrow(d), nrow=100)
    # pop_los = matrix(NA, ncol = nrow(d), nrow=100)
    # pop_his = matrix(NA, ncol = nrow(d), nrow=100)
    # for(i in 1:len(dfiles)) {
    #   dfolder = unlist(strsplit(dfiles[i],"_"))[2]
    #   dfolder = paste("sims",dfolder,"out",dfiles[i],sep="/")
    #   outfiles = list.files(dfolder)
    #   pop_file = outfiles[grep("POP", outfiles)]
    #   i_dat = read.csv(paste(dfolder,pop_file,sep="/"), header=T)
    #   i_dat$N[is.na(i_dat$N)] = 0
    #   pop_mn = tapply(i_dat$N, i_dat$YEAR, function(x) mean(x, na.rm=T))
    #   pop_lo = tapply(i_dat$N, i_dat$YEAR, function(x) quantile(x, probs = 0.75, na.rm=T))
    #   pop_hi = tapply(i_dat$N, i_dat$YEAR, function(x) quantile(x, probs = 0.25, na.rm=T))
    #   pop_mns[,i] = as.vector(pop_mn)
    #   pop_los[,i] = as.vector(pop_lo)
    #   pop_his[,i] = as.vector(pop_hi)
    # }
    # plot(1:nrow(pop_mns), pop_mns[,1], type = "n", ylim = c(0, max(pop_his)))
    # for(i in 1:ncol(pop_mns)) {
    #   xvals = 1:len(pop_his[,i])
    #   polygon( c(xvals,rev(xvals)),
    #            c(pop_los[,i],rev(pop_his[,i])),
    #            col = alpha(colrange[i],0.5), border = FALSE)
    # }
    # for(i in 1:ncol(pop_mns)) {
    #   xvals = 1:len(pop_his[,i])
    #   lines(xvals, pop_mns[,i], col = colrange[i], lwd = 2,
    #         xlab = xlab, ylab = ylab, cex.axis = cex.axis, cex.lab = cex.lab)
    # }
    # legend(x = 85, y = 1100, legend = c("Equal", "0.25", "0.5", "0.75"), fill = colrange)
    # 
  } else {
    if(is.null(ylim)) ylim = c(0,max(yval))
    
    pdata = barplot(yval, names = barlabs, col = colrange, ylab = ylab, ylim = ylim, 
                    cex.axis = cex.axis, cex.names = cex.lab)
    
    mtext("Land distribution", side = 1, line = 4, cex = cex.lab)  
  }
  
  
  
}
