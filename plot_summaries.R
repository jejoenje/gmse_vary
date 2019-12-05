
rm(list=ls())

library(RColorBrewer)
source("helpers.R")

out = read.csv("sims/sims_done.csv", header=T)

### Subsetting columns of interest:

out = subset(out, select = c("idx",
                             "yield_value",
                             "tend_crop_yld",
                             "public_land",
                             "man_bud_type",
                             "land_type",
                             "land_type_max_frac",
                             "ytb_type",
                             "trend_Min.",
                             "trend_Median",
                             "trend_Mean",
                             "trend_Max.",
                             "EXT",
                             "EXT_first"
                             ))


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

tcy = 0.2
pl = 0
mbt = "fixed"

par(oma=c(2,2,2,2))
par(mfrow=c(3,3))

lt = "equal"
ltmax = NULL
plot_trend_yrtn(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="Reds", ltmax = ltmax,
           xlab = "Year", ylab = "Mean population trend", ylim = c(0,1200))
plot_trend_yrtn(dat = out, y = "trend_mean", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="GnBu", ltmax = ltmax,
           xlab = "Yield return", ylab = "Mean population trend", ylim = c(0,1.2))
plot_trend_yrtn(dat = out, y = "ext_perc", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="Reds", ltmax = ltmax,
           xlab = "Yield return", ylab = "% Extinctions", ylim = c(0,100))

lt = "oneRich"
ltmax = 0.25
plot_trend_yrtn(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="Reds", ltmax = ltmax,
           xlab = "Year", ylab = "Mean population trend", ylim = c(0,1200))
plot_trend_yrtn(dat = out, y = "trend_mean", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="GnBu", ltmax = ltmax,
           xlab = "Yield return", ylab = "Mean population trend", ylim = c(0,1.2))
plot_trend_yrtn(dat = out, y = "ext_perc", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="Reds", ltmax = ltmax,
           xlab = "Yield return", ylab = "% Extinctions", ylim = c(0,100))

lt = "oneRich"
ltmax = 0.5
plot_trend_yrtn(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="Reds", ltmax = ltmax,
           xlab = "Year", ylab = "Mean population trend", ylim = c(0,1200))
plot_trend_yrtn(dat = out, y = "trend_mean", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="GnBu", ltmax = ltmax,
           xlab = "Yield return", ylab = "Mean population trend", ylim = c(0,1.2))
plot_trend_yrtn(dat = out, y = "ext_perc", tcy = tcy, pl = pl, mbt = mbt, lt = lt, col="Reds", ltmax = ltmax,
           xlab = "Yield return", ylab = "% Extinctions", ylim = c(0,100))

par(mfrow=c(2,2))
plot_trend_yrtn(dat = out, y = "trend_mean", tcy = 0.2, pl = 0, mbt = "mean", lt = "oneRich", col="GnBu", ltmax = 0.5,
           xlab = "Yield return", ylab = "Mean population trend", ylim = c(0,1.1))
plot_trend_yrtn(dat = out, y = "ext_perc", tcy = 0.2, pl = 0, mbt = "mean", lt = "oneRich", col="Oranges", ltmax = 0.5,
           xlab = "Yield return", ylab = "% Extinctions", ylim = c(0,100))

plot_trend_yrtn(dat = out, y = "trend_mean", tcy = 0.2, pl = 0.5, mbt = "mean", lt = "oneRich", col="GnBu", ltmax = 0.5,
           xlab = "Yield return", ylab = "Mean population trend", ylim = c(0,1.1))
plot_trend_yrtn(dat = out, y = "ext_perc", tcy = 0.2, pl = 0.5, mbt = "mean", lt = "oneRich", col="Oranges", ltmax = 0.5,
           xlab = "Yield return", ylab = "% Extinctions", ylim = c(0,100))


dat = out
tcy = 0.2
pl = 0
mbt = "mean"

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
      dfolder = unlist(strsplit(dfiles[i],"_"))[2]
      dfolder = paste("sims",dfolder,"out",dfiles[i],sep="/")
      outfiles = list.files(dfolder)
      pop_file = outfiles[grep("POP", outfiles)]
      i_dat = read.csv(paste(dfolder,pop_file,sep="/"), header=T)
      pop_dat[[i]] = i_dat
    }
    lo = min(unlist(lapply(pop_dat, function(x) min(x, na.rm=T) )))
    hi = max(unlist(lapply(pop_dat, function(x) max(x, na.rm=T) )))
    ylims = c(lo,hi)

    plot(pop_dat[[1]]$YEAR, pop_dat[[1]]$N, ylim = ylims, type = "n")

    for(i in 1:nrow(d)) {
      i_dat = pop_dat[[i]]
      for(j in 1:nlevels(factor(i_dat$SIM))) {
        i_dat_j = i_dat[i_dat$SIM == j,]
        lines(i_dat_j$YEAR, i_dat_j$N, col = alpha(colrange[i], 0.5))
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


par(mfrow=c(2,3))
yv = 0.4
mbt = "fixed"
plot_summary(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100))
mbt = "mean"
plot_summary(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100))
mbt = "max"
plot_summary(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100))


mbt = "fixed"
plot_summary(dat = out, y = "ext_perc", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100))
mbt = "mean"
plot_summary(dat = out, y = "ext_perc", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100))
mbt = "max"
plot_summary(dat = out, y = "ext_perc", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100))




mbt = "fixed"
plot_summary(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100), 
             xlab = "Year", ylab = "Population size")
mbt = "mean"
plot_summary(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100), 
             xlab = "Year", ylab = "Population size")
mbt = "max"
plot_summary(dat = out, y = "trends", tcy = tcy, pl = pl, mbt = mbt, yv = yv, col = "BrBG", ylim = c(0,100), 
             xlab = "Year", ylab = "Population size")






par(mfrow=c(1,2))
plot_trend(dat = out, y = "trend_mean", tcy = 0.2, pl = 0, mbt = "fixed", lt = "oneRich", ltmax = 0.5, col="GnBu", 
           xlab = "Yield return", ylab = "Mean population trend")
plot_trend(dat = out, y = "ext_perc", tcy = 0.2, pl = 0, mbt = "fixed", lt = "oneRich", ltmax = 0.5, col="Oranges", 
           xlab = "Yield return", ylab = "% Extinction")


plot_trend(dat = out, tcy = 0.2, pl = 0, mbt = "mean", lt = "oneRich", ltmax = 0.5, col="GnBu")
plot_trend(dat = out, tcy = 0.2, pl = 0, mbt = "max", lt = "oneRich", ltmax = 0.5, col="GnBu")






### 
### SUMMARY PLOTS - TRENDS AND EXTINCTIONS:
###
trd_ext_plots = function(dat, type = "mean", mbt = "max", pub_land = 0, max_frac = 0.5, collapse_tcy = NULL) {
  
  ### Subset by selected pub_land and max_frac:
  dat = dat[dat$public_land == pub_land & dat$land_type_max_frac == max_frac,]
  
  ### Split data by man_bud_type (only looking at the two for now):
  dat_mbt_fixed = dat[dat$man_bud_type=="fixed",]
  dat_mbt = dat[dat$man_bud_type==mbt,]
  
  ### Set up plotting area:
  par(oma=c(3,3,4,0))
  par(mfrow=c(2,2))
  
  if(is.null(collapse_tcy)) {
    
    ### Pick some colors for sets of trends and extinction probability:
    col_ext = brewer.pal(3, "Oranges")
    col_trd = brewer.pal(3, "YlGnBu")
    
    if(type == "mean") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Mean
      dat_mbt$plotval = dat_mbt$trend_Mean
    }
    
    if(type == "median") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Median
      dat_mbt$plotval = dat_mbt$trend_Median
    }
    
    par(mar=c(2,3,2,2))
    barplot(plotval ~ tend_crop_yld + yield_value, data = dat_mbt_fixed, beside = T, ylim = c(0, 1.25),
            xaxt ="n", xlab="", col = col_trd)
    axis(1, at = c(2.5,6.5,10.5,14.5), labels = F)
    abline(h = 1, col = "red", lty = "dashed")
    
    par(mar=c(2,2,2,3))
    barplot(plotval ~ tend_crop_yld + yield_value, data = dat_mbt, beside = T, ylim = c(0, 1.25),
            xaxt = "n", yaxt = "n", xlab="", ylab = "", col = col_trd)
    axis(1, at = c(2.5,6.5,10.5,14.5), labels = F)
    axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2), labels = F)
    abline(h = 1, col = "red", lty = "dashed")
    
    par(mar=c(3,3,1,2))
    barplot(EXT ~ tend_crop_yld + yield_value, data = dat_mbt_fixed, beside = T, 
            ylim = c(0, 100), col = col_ext)
    axis(1, at = c(2.5,6.5,10.5,14.5), labels = F)
    
    par(mar=c(3,2,1,3))
    barplot(EXT ~ tend_crop_yld + yield_value, 
            data = dat_mbt, beside = T, ylim = c(0,100), yaxt = "n", col = col_ext)
    axis(1, at = c(2.5,6.5,10.5,14.5), labels = F)
    axis(2, at = c(0,20,40,60,80,100), labels = F)
    
    mtext(outer = T, "Yield return", 1, line = 1, cex = 1.25)
    mtext(outer = T, "Mean pop. trend", 2, at = 0.75, line = 1, cex = 1.25)
    mtext(outer = T, "% extinctions", 2, at = 0.3, line = 1, cex = 1.25)
    mtext(outer = T, "Manager budget", 3, at = 0.5, line = 2, cex = 1.25)
    mtext(outer = T, "Fixed", 3, at = 0.3, line = 0, cex = 1.25)
    
    if(mbt == "max") mbt_label = "User max."
    if(mbt == "mean") mbt_label = "User mean"
    mtext(outer = T, mbt_label, 3, at = 0.75, line = 0, cex = 1.25)
  }
  
  if(!is.null(collapse_tcy)) {
    ### Pick some colors for sets of trends and extinction probability:
    col_ext = brewer.pal(3, "Oranges")[3]
    col_trd = brewer.pal(3, "YlGnBu")[3]
    
    ### Pick a single value for tcy (tend_crop_yield) to plot:
    dat_mbt_fixed = dat_mbt_fixed[dat_mbt_fixed$tend_crop_yld==0.2,]
    dat_mbt = dat_mbt[dat_mbt$tend_crop_yld==0.2,]
    
    if(type == "mean") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Mean
      dat_mbt$plotval = dat_mbt$trend_Mean
    }
    
    if(type == "median") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Median
      dat_mbt$plotval = dat_mbt$trend_Median
    }
    
    par(mar=c(2,3,2,2))
    barplot(plotval ~ yield_value, data = dat_mbt_fixed, beside = T, ylim = c(0, 1.25),
            xaxt ="n", xlab="", col = col_trd)
    axis(1, at = c(1,2,3,4), labels = F)
    abline(h = 1, col = "red", lty = "dashed")
    
    par(mar=c(2,2,2,3))
    barplot(plotval ~ yield_value, data = dat_mbt, beside = T, ylim = c(0, 1.25),
            xaxt = "n", yaxt = "n", xlab="", ylab = "", col = col_trd)
    axis(1, at = c(1,2,3,4), labels = F)
    axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2), labels = F)
    abline(h = 1, col = "red", lty = "dashed")
    
    par(mar=c(3,3,1,2))
    barplot(EXT ~ yield_value, data = dat_mbt_fixed, beside = T, 
            ylim = c(0, 100), col = col_ext)
    axis(1, at = c(1,2,3,4), labels = F)
    
    par(mar=c(3,2,1,3))
    barplot(EXT ~ yield_value, 
            data = dat_mbt, beside = T, ylim = c(0,100), yaxt = "n", col = col_ext)
    axis(1, at = c(1,2,3,4), labels = F)
    axis(2, at = c(0,20,40,60,80,100), labels = F)
    
    mtext(outer = T, "Yield return", 1, line = 1, cex = 1.25)
    mtext(outer = T, "Mean pop. trend", 2, at = 0.75, line = 1, cex = 1.25)
    mtext(outer = T, "% extinctions", 2, at = 0.3, line = 1, cex = 1.25)
    mtext(outer = T, "Manager budget", 3, at = 0.5, line = 2, cex = 1.25)
    mtext(outer = T, "Fixed", 3, at = 0.3, line = 0, cex = 1.25)
    
    if(mbt == "max") mbt_label = "User max."
    if(mbt == "mean") mbt_label = "User mean"
    mtext(outer = T, mbt_label, 3, at = 0.75, line = 0, cex = 1.25)
    
  }
  
  
}



### List available values:
table(out$yield_value)
table(out$public_land)
table(out$land_type)
table(out$land_type_max_frac)

pub_land = 0
max_frac = 0.5

# Plot everyhing:
trd_ext_plots(out, type = "mean", mbt = "max", pub_land = pub_land, max_frac = max_frac)
trd_ext_plots(out, type = "mean", mbt = "mean", pub_land = pub_land, max_frac = max_frac)

# Plot but collapse by given tend_crop_yld value:
trd_ext_plots(out, type = "mean", mbt = "max", pub_land = pub_land, max_frac = max_frac, collapse_tcy = 0.2)
trd_ext_plots(out, type = "mean", mbt = "mean", pub_land = pub_land, max_frac = max_frac, collapse_tcy = 0.2)





### POPULATION TREND PLOTS:

### Pick same subsets as above:

pop_trends_plot = function(dat, tcy=NULL, mbt = "fixed", max_frac = 0.5, pub_land = 0, ylim = NULL) {

  ### Subset sim data by given pub_land and max_frac:
  dat = dat[c(dat$public_land == pub_land & dat$land_type_max_frac == max_frac),]
  
  ### Set tend_crop_yield to default if not given:
  if(is.null(tcy)) { tcy = 0.2 }

  ### Subset further by given manager budget type and tend_crop_yld:
  dat = dat[dat$man_bud_type==mbt & dat$tend_crop_yld==tcy,]
  
  para_sets = nrow(dat)
  sims_idx = as.vector(dat$idx)
  
  ### Load list of original output: 
  outdir = "sims/nullModel-YTB4/out/"
  
  par(mfrow=c(2,2))
  par(oma=c(5,3,0,0))
  cols = brewer.pal(8, "YlOrRd")
  cols = tail(cols,4)
  
  xax = c("n","n","s","s")
  yax = c("s","n","s","n")
  
  mars = matrix(c(1,3,1,1,
                  1,1,1,3,
                  1,3,1,1,
                  1,1,1,3), nrow=4, ncol=4, byrow=T)
  
  for(i in 1:para_sets) {
    i_files = list.files(paste(outdir, sims_idx[i],sep=""))
    i_fname = i_files[grep("POP", i_files)]
    POP = read.csv(paste(outdir, sims_idx[i], "/",i_fname, sep=""),header=T)
    sims = max(POP$SIM)
  
    par(mar=mars[i,])
  
    if(is.null(ylim)) {
      ylim = c(0,1500)
    }
    
    plot(1:max(POP$YEAR), POP$N[POP$SIM==1], type = "n", 
         ylim = ylim, 
         xaxt = xax[i], yaxt = yax[i])
    if(i == 1 | i == 2) { axis(1, at = c(0,20,40,60,80,100), labels = FALSE) }
    if(i == 2 | i == 4) { axis(2, at = c(0, 200, 400,600,800,1000,1200), labels = FALSE)  }
    
    for(j in 1:sims) {
      POP_i = POP[POP$SIM == j, ]
      lines(1:max(POP_i$YEAR), POP_i$N, col = alpha(cols[i], 0.5))
    }
  
  }
  
  mtext("Year", outer = T, side = 1, line = 3, cex = 1.5)
  mtext("Population size", outer = T, side = 2, line = 1, cex = 1.5)
  
}

pop_trends_plot(dat = out, mbt = "fixed", pub_land = 0, ylim = c(0,1200))
pop_trends_plot(dat = out, mbt = "mean", pub_land = 0, ylim = c(0,1200))
pop_trends_plot(dat = out, mbt = "max", pub_land = 0, ylim = c(0,1200))










