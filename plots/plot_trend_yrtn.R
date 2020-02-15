### This plots summary graphs of population trajectories, mean trends and prop. extinctions as related to 
###  yield_value, for given other parameters


plot_trend_yrtn = function(dat, y, tcy=NULL, pl=NULL, mbt=NULL, lt=NULL, ltmax=NULL, col="GnBu", yaxt = "s", cex.axis = 1.5,
                           xlab="",ylab="", cex.lab = 1.5, ylim = NULL) {
  
  if(is.null(dat)) stop("Missing data!")
  
  paras_names = c("tcy", "pl", "mbt", "lt", "ltmax")
  check_paras = c(is.null(tcy), is.null(pl), is.null(mbt), is.null(lt))
  if(sum(check_paras)>0) {
    missing = paste(paras_names[check_paras],collapse = ", ")
    stop(paste("Missing input parameters:", missing))
  }
  
  # Create selector which allows for ltmax to be NULL (if null, ignore it)
  
  if(is.null(ltmax)) {
    d = dat[dat$tend_crop_yld==tcy & 
              dat$public_land==pl & 
              dat$man_bud_type==mbt & 
              dat$land_type==lt,]
  } 
  if(!is.null(ltmax)) {
    d = dat[dat$tend_crop_yld==tcy & 
              dat$public_land==pl & 
              dat$man_bud_type==mbt & 
              dat$land_type==lt & 
              dat$land_type_max_frac==ltmax,]
  }
  
  if(is.null(col)) col = "GnBu"
  
  mycols = tail(brewer.pal(3, col),1)
  
  if(y == "trend_mean") ydata = d$trend_Mean
  if(y == "ext_perc") ydata = d$EXT
  
  if(y == "trends") {
    dfiles = as.vector(d$idx)
    pop_dat = matrix(NA, ncol = nrow(d), nrow=100) 
    for(i in 1:len(dfiles)) {
      dfolder = unlist(strsplit(dfiles[i],"_"))[2]
      dfolder = paste("sims",dfolder,"out",dfiles[i],sep="/")
      outfiles = list.files(dfolder)
      pop_file = outfiles[grep("POP", outfiles)]
      i_dat = read.csv(paste(dfolder,pop_file,sep="/"), header=T)
      pop_mean = tapply(i_dat$N, i_dat$YEAR, function(x) mean(x, na.rm=T))
      pop_dat[,i] = as.vector(pop_mean)
    }
    
    colrange = tail(brewer.pal(8, col), ncol(pop_dat))
    colrange = colrange[order(colrange, decreasing=T)]
    par(mar=c(4.5,4.5,1.5,0.5))
    if(is.null(ylim)) { ylim = c(0,max(pop_dat)) }
    if(is.null(ylab)) { ylab = "Mean population trend" }
    if(is.null(xlab)) { xlab = "year" }
    plot(1:nrow(pop_dat), pop_dat[,1], type = "n", ylim=ylim, xlab = xlab, ylab = ylab, 
         cex.axis = cex.axis, cex.lab = cex.lab)
    for(i in 1:ncol(pop_dat)) {
      lines(1:len(pop_dat[,i]), pop_dat[,i], col = colrange[i], lwd = 3)
    }
    
  } else {
    if(is.null(ylim)) ylim = c(0, max(ydata)*1.125)
    
    #par(oma=c(0,0,0,0))
    par(mar=c(4.5,4.5,1.5,0.5))
    barplot(ydata ~ d$yield_value + d$tend_crop_yld, names = d$yield_value, 
            beside =T, col = mycols, yaxt = yaxt, cex.axis = cex.axis, cex.names = cex.axis, cex.lab = cex.lab,
            ylab = ylab, xlab=xlab, space = 0.1, ylim = ylim)    
  }
  
  
  
  
  
  
  
}
