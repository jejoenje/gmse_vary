library(scales)
gmse_iter_plot = function(gmse_iter_dat) {
  
  yrs = nrow(gmse_iter_dat[[1]])
  
  min_pop = min(gmse_iter_dat[[1]]$Pop_size)
  max_pop = max(gmse_iter_dat[[1]]$Pop_size)
  
  if(gmse_paras$manage_target > max_pop) max_pop = gmse_paras$manage_target*1.1
  if(gmse_paras$manage_target < min_pop) min_pop = gmse_paras$manage_target*0.9
  
  ### Unpack AGENTS and ACTIONS data
  AGENTS = gmse_iter_dat[[2]]
  ACTIONS = gmse_iter_dat[[3]]
  
  ### Unpack ACTIONS further
  UACTIONS =lapply(ACTIONS, function(x) x[2:nrow(x),2:ncol(x)])
  sum_uactions = lapply(UACTIONS, function(x) apply(x,2,sum))
  sum_uactions = matrix(unlist(sum_uactions), ncol=length(sum_uactions[[1]]), byrow=TRUE)
  colnames(sum_uactions) = colnames(UACTIONS[[1]])
  # Keep only those user action columns that are not all NA:
  noactions = apply(is.na(sum_uactions),2,sum)==nrow(sum_uactions)
  sum_uactions = sum_uactions[,!noactions]
  
  ### Set up plotting region
  par(mfrow=c(1,2))
  
  ### Plot pop trend
  par(mar=c(5,5,3,5))
  plot(gmse_iter_dat[[1]]$Time, gmse_iter_dat[[1]]$Pop_est, type="l", col=alpha("black",0.75),
       ylim=c(min_pop, max_pop), ylab = "Population size", xlab = "Time")
  abline(h = gmse_paras$manage_target, col = "red", lty="dashed", lwd=2)
  
  ### Add yields
  # Calc ranges

  ylds = lapply(AGENTS, function(x) {
    x[2:nrow(x),16]
  })
  ylds = matrix(unlist(ylds), ncol = length(ylds[[1]]), byrow =T)
  max_yield = max(ylds)
  min_yield = min(ylds)
  
  par(new = T)
  plot(1:nrow(ylds), ylds[,1], 
       type="n", col = "grey", axes = F, xlab="", ylab="", ylim=c(min_yield,max_yield))  
  
  for(i in 1:ncol(ylds)){
    lines(1:nrow(ylds), ylds[,i], col = "blue")  
  }
  
  axis(side = 4, col = "grey")
  mtext(side = 4, line = 3, 'Yield per user', col = "blue")
  
  ### Plot actions
  # Reorganise so "culling" is always the first code:
  sum_uactions = cbind(sum_uactions[,which(colnames(sum_uactions)=="culling")],sum_uactions[,which(colnames(sum_uactions)!="culling")])
  colnames(sum_uactions)[1] = "culling"
  barplot(t(sum_uactions), border=NA, col=brewer.pal(ncol(sum_uactions),"RdYlBu"),space=0)
  axis(1)
  mtext(side =1, line = 3, "Time")
}