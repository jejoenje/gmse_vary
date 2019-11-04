### Summarising functions, plotting etc.
### 
### 

### extract_gmse()
### 
### Takes a list as argument, which is one or more series of outputs (sims) of gmse_apply() with all results saved.
### For each sim, extracts the given result vector from the output.

extract_gmse = function(all_dat, extract = "resource_results") {
  no_sims = len(all_dat)
  no_years = length(all_dat[[1]][])
  dat = matrix(NA, nrow=no_sims, ncol=no_years)
  for(i in 1:no_sims) {
    if(extract == "resource_results") {
      dat[i,] = unlist(lapply(all_dat[[i]], function(x) x$basic_output$resource_results))
    }
    if(extract == "obs_results") {
      dat[i,] = unlist(lapply(all_dat[[i]], function(x) x$basic_output$observation_results))
    }
  }
  return(dat)
}

### plot_sims()
### 
### Plots output of gmse_apply() sim results.
### 
### Can plot different types of output - 
### - resource: "true" resource only
### - observation: "observed" resources
### - resobs: both true and observed resources in the same plot.

plot_sims = function(gmse_res, type="resource", sumtype="mean", ylim=NULL) {
  
  ### Plot "real" resource number only:
  
  if(type=="resource") {
    
    y_res = extract_gmse(gmse_res, "resource_results")
    manage_target = gmse_res[[1]][[1]]$manage_target
    
    # Summarised as means and upper/lower bounds across simulations:
    
    if(sumtype == "mean") {
      
      x = 1:ncol(y_res)
      y_mean = apply(y_res, 2, mean)
      y_lo = apply(y_res, 2, function(x) quantile(x, probs = 0.025))
      y_hi = apply(y_res, 2, function(x) quantile(x, probs = 0.975))
      
      if(is.null(ylim)) {
        y_min = bufRange(y_lo, end="lo", incl_val = manage_target )
        y_max = bufRange(y_hi, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type="n", ylim = c(y_min, y_max))
      lines(x = x, y = y_mean, lwd = 2)
      lines(x = x, y = y_lo, lwd = 1, col="grey")
      lines(x = x, y = y_hi, lwd = 1, col="grey")
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
    # Not summarised, just each simulation plotted as a line
    
    if(sumtype == "none") {
      x = 1:ncol(y_res)
      
      if(is.null(ylim)) {
        y_min = bufRange(y_res, end="lo", incl_val = manage_target )
        y_max = bufRange(y_res, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type = "n", ylim=c(y_min, y_max))
      for(i in 1:nrow(y_res)) {
        lines(x, y_res[i,])
      }
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
  }
  
  if(type=="resobs") {
    
    y_res = extract_gmse(gmse_res, "resource_results")
    y_obs = extract_gmse(gmse_res, "obs_results")
    
    manage_target = gmse_res[[1]][[1]]$manage_target
    
    if(sumtype == "mean") {
      
      x = 1:ncol(y_res)
      y_res_mean = apply(y_res, 2, mean)
      y_obs_mean = apply(y_obs, 2, mean)
      
      y_res_lo = apply(y_res, 2, function(x) quantile(x, probs = 0.025))
      y_res_hi = apply(y_res, 2, function(x) quantile(x, probs = 0.975))
      y_obs_lo = apply(y_obs, 2, function(x) quantile(x, probs = 0.025))
      y_obs_hi = apply(y_obs, 2, function(x) quantile(x, probs = 0.975))
      
      if(is.null(ylim)) {
        y_min = bufRange(c(y_res_lo, y_obs_lo), end="lo", incl_val = manage_target )
        y_max = bufRange(c(y_res_hi, y_obs_hi), end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type="n", ylim = c(y_min, y_max))
      lines(x = x, y = y_res_mean, lwd = 2)
      lines(x = x, y = y_obs_mean, lwd = 2, col = "blue")
      
      lines(x = x, y = y_res_lo, lwd = 1, col="darkgrey")
      lines(x = x, y = y_res_hi, lwd = 1, col="darkgrey")
      
      lines(x = x, y = y_obs_lo, lwd = 1, col="blue")
      lines(x = x, y = y_obs_hi, lwd = 1, col="blue")
      
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
    if(sumtype == "none") {
      x = 1:ncol(y_res)
      
      if(is.null(ylim)) {
        y_min = bufRange(c(y_res,y_obs), end="lo", incl_val = manage_target )
        y_max = bufRange(c(y_res,y_obs), end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type = "n", ylim=c(y_min, y_max))
      for(i in 1:nrow(y_res)) {
        lines(x, y_res[i,], col = "black")
        lines(x, y_obs[i,], col = "blue")
      }
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
  }
  
  if(type=="observation") {
    
    y_obs = extract_gmse(gmse_res, "obs_results")
    
    manage_target = gmse_res[[1]][[1]]$manage_target
    
    if(sumtype == "mean") {
      
      x = 1:ncol(y_obs)
      y_obs_mean = apply(y_obs, 2, mean)
      
      y_obs_lo = apply(y_obs, 2, function(x) quantile(x, probs = 0.025))
      y_obs_hi = apply(y_obs, 2, function(x) quantile(x, probs = 0.975))
      
      if(is.null(ylim)) {
        y_min = bufRange(y_obs_lo, end="lo", incl_val = manage_target )
        y_max = bufRange(y_obs_hi, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_obs[1,], type="n", ylim = c(y_min, y_max))
      lines(x = x, y = y_obs_mean, lwd = 2, col = "blue")
      
      lines(x = x, y = y_obs_lo, lwd = 1, col="blue")
      lines(x = x, y = y_obs_hi, lwd = 1, col="blue")
      
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
    if(sumtype == "none") {
      x = 1:ncol(y_obs)
      
      if(is.null(ylim)) {
        y_min = bufRange(y_obs, end="lo", incl_val = manage_target )
        y_max = bufRange(y_obs, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_obs[1,], type = "n", ylim=c(y_min, y_max))
      for(i in 1:nrow(y_obs)) {
        lines(x, y_obs[i,], col = "blue")
      }
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
  }
  
}