plot_gmse_sims <- function(s, t = target, scl = sc) {
  t_sc <- t/scl
  
  y_min <- min(c(s$pop,t_sc))
  y_max <- max(c(s$pop,t_sc))
  y_min <- y_min-y_min*0.05
  y_max <- y_max+y_max*0.05
  
  plot(s$time, s$pop, type="l", ylim=c(y_min, y_max))
  
  abline(h = t_sc, col="red", lty="dashed")
  par(new = T)
  useryield <- s[,grep("yield",names(s))]
  plot(s$time, useryield[,1], col='grey', axes = F, type='n', xlab="",ylab="", ylim=c(min(useryield), max(useryield)) )
  for(i in 1:ncol(useryield)) {
    lines(s$time, useryield[,i], col='grey')
  }
  axis(side = 4)
  mtext(side = 4, line = 3, 'Yield per user')
}