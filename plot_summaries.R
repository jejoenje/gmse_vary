rm(list=ls())

out = read.csv("sims/sims_done.csv", header=T)

names(out)

### Subsetting columns of interest:

out = subset(out, select = c("yield_value",
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

trd_ext_plots = function(dat, type = "mean", pub_land = 0, max_frac = 0.5, collapse_tcy = NULL) {
  
  ### Subset by selected pub_land and max_frac:
  dat = dat[dat$public_land == pub_land & dat$land_type_max_frac == max_frac,]
  
  ### Split data by man_bud_type (only looking at the two for now):
  dat_mbt_fixed = dat[dat$man_bud_type=="fixed",]
  dat_mbt_max = dat[dat$man_bud_type=="mean",]
  
  ### Set up plotting area:
  par(oma=c(3,3,4,0))
  par(mfrow=c(2,2))
  
  if(is.null(collapse_tcy)) {
    
    ### Pick some colors for sets of trends and extinction probability:
    col_ext = brewer.pal(3, "Oranges")
    col_trd = brewer.pal(3, "YlGnBu")
    
    if(type == "mean") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Mean
      dat_mbt_max$plotval = dat_mbt_max$trend_Mean
    }
    
    if(type == "median") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Median
      dat_mbt_max$plotval = dat_mbt_max$trend_Median
    }
    
    par(mar=c(2,3,2,2))
    barplot(plotval ~ tend_crop_yld + yield_value, data = dat_mbt_fixed, beside = T, ylim = c(0, 1.25),
            xaxt ="n", xlab="", col = col_trd)
    axis(1, at = c(2.5,6.5,10.5,14.5), labels = F)
    abline(h = 1, col = "red", lty = "dashed")
    
    par(mar=c(2,2,2,3))
    barplot(plotval ~ tend_crop_yld + yield_value, data = dat_mbt_max, beside = T, ylim = c(0, 1.25),
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
            data = dat_mbt_max, beside = T, ylim = c(0,100), yaxt = "n", col = col_ext)
    axis(1, at = c(2.5,6.5,10.5,14.5), labels = F)
    axis(2, at = c(0,20,40,60,80,100), labels = F)
    
    mtext(outer = T, "Yield return", 1, line = 1, cex = 1.25)
    mtext(outer = T, "Mean pop. trend", 2, at = 0.75, line = 1, cex = 1.25)
    mtext(outer = T, "% extinctions", 2, at = 0.3, line = 1, cex = 1.25)
    mtext(outer = T, "Manager budget", 3, at = 0.5, line = 2, cex = 1.25)
    mtext(outer = T, "Fixed", 3, at = 0.3, line = 0, cex = 1.25)
    mtext(outer = T, "User max.", 3, at = 0.75, line = 0, cex = 1.25)
    
  }
  
  if(!is.null(collapse_tcy)) {
    ### Pick some colors for sets of trends and extinction probability:
    col_ext = brewer.pal(3, "Oranges")[3]
    col_trd = brewer.pal(3, "YlGnBu")[3]
    
    ### Pick a single value for tcy (tend_crop_yield) to plot:
    dat_mbt_fixed = dat_mbt_fixed[dat_mbt_fixed$tend_crop_yld==0.2,]
    dat_mbt_max = dat_mbt_max[dat_mbt_max$tend_crop_yld==0.2,]
    
    if(type == "mean") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Mean
      dat_mbt_max$plotval = dat_mbt_max$trend_Mean
    }
    
    if(type == "median") {
      dat_mbt_fixed$plotval = dat_mbt_fixed$trend_Median
      dat_mbt_max$plotval = dat_mbt_max$trend_Median
    }
    
    par(mar=c(2,3,2,2))
    barplot(plotval ~ yield_value, data = dat_mbt_fixed, beside = T, ylim = c(0, 1.25),
            xaxt ="n", xlab="", col = col_trd)
    axis(1, at = c(1,2,3,4), labels = F)
    abline(h = 1, col = "red", lty = "dashed")
    
    par(mar=c(2,2,2,3))
    barplot(plotval ~ yield_value, data = dat_mbt_max, beside = T, ylim = c(0, 1.25),
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
            data = dat_mbt_max, beside = T, ylim = c(0,100), yaxt = "n", col = col_ext)
    axis(1, at = c(1,2,3,4), labels = F)
    axis(2, at = c(0,20,40,60,80,100), labels = F)
    
    mtext(outer = T, "Yield return", 1, line = 1, cex = 1.25)
    mtext(outer = T, "Mean pop. trend", 2, at = 0.75, line = 1, cex = 1.25)
    mtext(outer = T, "% extinctions", 2, at = 0.3, line = 1, cex = 1.25)
    mtext(outer = T, "Manager budget", 3, at = 0.5, line = 2, cex = 1.25)
    mtext(outer = T, "Fixed", 3, at = 0.3, line = 0, cex = 1.25)
    mtext(outer = T, "User max.", 3, at = 0.75, line = 0, cex = 1.25)
    
  }
  
    
}

### List available values:
table(out$yield_value)
table(out$public_land)
table(out$land_type)
table(out$land_type_max_frac)

# Plot everyhing:
trd_ext_plots(out, type = "mean", pub_land = 0,    max_frac = 0.5)

# again, but for different pub_land values:
trd_ext_plots(out, type = "mean", pub_land = 0.25, max_frac = 0.5)

# Plot but collapse by given tend_crop_yld value:
trd_ext_plots(out, type = "mean", pub_land = 0,    max_frac = 0.5, collapse_tcy = 0.2)





