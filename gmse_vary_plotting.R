gmse_vary_plotting = function(folders, plot_sims = 20) {

  par(mfrow=c(length(folders),3))
  par(mar = c(4,4,0.5,0.5))

  source("helpers.R")
  source("gmse_apply_helpers.R")
  
  extinction_summary = as.data.frame(NULL)
  
  read_number = plot_sims
  
  for(i in 1:length(folders)) {
    folder = folders[i]
    sims = gmse_rds_summary(folder, read_number = read_number)
    gmse_vary_plot(sims, type = "pop", col = "darkred", 
                   ylab = "Population", xlab = "Time step", ctext = paste("Scenario",i))
    gmse_vary_plot(sims, type = "yield_usermean", col = "darkgreen", sumtype = "median", 
                   ylab = "Yield", xlab = "Time step", ctext = paste("Scenario",i))
    gmse_vary_plot(sims, type = "budget_usermean", col = "darkblue", sumtype = "median", 
                   ylab = "Budget", xlab = "Time step", ctext = paste("Scenario",i))
    
    extinction_count = sum(unlist(lapply(sims, function(x) nrow(x$pop)<x$par$n_years+1)))
    extinction_frac = extinction_count/length(sims)
    
    extinction_summary = rbind(extinction_summary, cbind(folder, extinction_frac))
    
    #save(extinction_frac, file = paste(folder, "extinction_frac.Rdata", sep = "/"))
    rm(sims, folder, extinction_count, extinction_frac)  
  }
  
  return(extinction_summary)

}
