### Alias for length()
len = length

### Return number of values (len()) and number of unique values in vector:
ulen = function(x) {
  return(c(len(x), len(unique(x))))
}

### bufRange()
### 
### Takes a range of values and returns either the maximum (end="hi") or minimum (end="lo") of the range,
### plus or minus a given buffer fraction.
### If incl_val != NULL, extends either the min/max range to include a given constant.
bufRange = function(limrange, end, buffer = 0.05, incl_val=NULL) {
  if(end == "hi") {
    val = max(limrange)+max(limrange)*buffer
    if(incl_val>val) val = incl_val+incl_val*0.05
    val = ceiling(val)
  }
  if(end == "lo") {
    val = min(limrange)-min(limrange)*buffer
    if(incl_val<val) val = incl_val-incl_val*0.05
    val = floor(val)
  }
  return(val)
}


saved <- function(dat, prefix="") {
  t <- as.character(Sys.time())
  t <- gsub(" ", "", t, fixed = TRUE)
  t <- gsub("-", "", t, fixed = TRUE)
  t <- gsub(":", "", t, fixed = TRUE)
  outname <- paste("./out/",prefix,"_",t,".Rdata",sep="")
  save(dat, file=outname)
}

plot_gmse_sims <- function(s, t = target, scl = sc, main = "") {
  t_sc <- t/scl
  
  y_min <- min(c(s$pop,t_sc))
  y_max <- max(c(s$pop,t_sc))
  y_min <- y_min-y_min*0.05
  y_max <- y_max+y_max*0.05
  
  par(mar=c(5,5,3,5))
  plot(s$time, s$pop, type="l", ylim=c(y_min, y_max), ylab="", xlab = "Time step", main = main)
  
  abline(h = t_sc, col="red", lty="dashed")
  par(new = T)
  useryield <- s[,grep("yield",names(s))]
  plot(s$time, useryield[,1], col='grey', axes = F, type='n', xlab="",ylab="", ylim=c(min(useryield), max(useryield)) )
  for(i in 1:ncol(useryield)) {
    lines(s$time, useryield[,i], col='grey')
  }
  axis(side = 4)
  mtext(side = 4, line = 3, 'Yield per user')
  mtext(side = 2, line = 3, 'Population size (est)')
  
}