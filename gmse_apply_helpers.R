### Summarising functions, plotting etc.
### 
### 

### get_user_acts()
### 
### Extracts a list of matrixes where each list element is a simulation, each row in a matrix is a year,
###  and each column in a matrix is a user. Each value is the desired user parameter (currently: number 
###  of actions taken (cull, scare, tend crops), yield, or budget.)

get_user_data = function(all_dat, type) {
  
  no_sims = len(all_dat)
  no_years = max(unlist(lapply(all_dat, len)))
  
  dat = list()
  
  if(type == "culls" | type == "scares" | type == "crops") {
    
    for(i in 1:no_sims) {
      # Find no. of stakeholders
      stakeholders = all_dat[[i]][[1]]$stakeholders
      
      # For each year, within a sim ulation...
      
      udat_j = matrix(NA, ncol=stakeholders, nrow=no_years)
      
      for(j in 1:no_years) {
        
        if(class(all_dat[[i]][[j]])=="list") {
          # Extract actions for year j in sim i
          udat_year = all_dat[[i]][[j]]$ACTION
          user_dat = udat_year[,,2:dim(udat_year)[3]] 
          udat = as.vector(NULL)
          for(k in 1:stakeholders) {
            if(type == "culls") {
              udat[k] = user_dat[,,k][1,9] 
            }
            if(type == "scares") {
              udat[k] = user_dat[,,k][1,8]
            }
            if(type == "crops") {
              udat[k] = user_dat[,,k][2,10]  
            }
          }
          udat_j[j,] = udat
        }
      }
      dat[[i]] = udat_j
    }
    return(dat)
  }
  
  if(type == "yield" | type == "budget") {
    
    for(i in 1:no_sims) {
      # Find no. of stakeholders
      stakeholders = all_dat[[i]][[1]]$stakeholders
      
      # For each year, within a simulation...
      
      udat_j = matrix(NA, ncol=stakeholders, nrow=no_years)
      
      for(j in 1:no_years) {
        
        if(class(all_dat[[i]][[j]])=="list") {
        
          # Extract actions for year j in sim i
          udat_year = all_dat[[i]][[j]]$AGENTS
          if(type == "yield") {
            udat_j[j,] = udat_year[2:nrow(udat_year),16]
          }
          if(type == "budget") {
            udat_j[j,] = udat_year[2:nrow(udat_year),17]
          }
        }
      }
      dat[[i]] = udat_j
    }
    return(dat)    ## Returning data if type == "yield" or "budget"
  }

}

extract_gmse = function(all_dat, extract = "resources") {
  
  accepted_types = c("resources","observations","culls", "scares", "crops", "yield", "budget")
  if(!(extract %in% accepted_types)) {
    stop(sprintf("No extraction method for user data type '%s'. ", extract))
  }
  
  no_sims = len(all_dat)
  no_years = max(unlist(lapply(all_dat, len)))
  
  if(extract == "resources") {
    dat = matrix(NA, nrow=no_sims, ncol=no_years)
    for(i in 1:no_sims) {
      #dat[i,] = unlist(lapply(all_dat[[i]], function(x) x$basic_output$resource_results))
      dat[i,] = unlist(
        lapply(all_dat[[i]], function(x) {
            if(class(x) == "character") {
              return(NA)
            } else {
              return(x$basic_output$resource_results)  
            }
          })
        )
    }
    return(dat)
  }
  
  if(extract == "observations") {
    dat = matrix(NA, nrow=no_sims, ncol=no_years)
    for(i in 1:no_sims) {
      #dat[i,] = unlist(lapply(all_dat[[i]], function(x) x$basic_output$observation_results))
      dat[i,] = unlist(
        lapply(all_dat[[i]], function(x) {
          if(class(x) == "character") {
            return(NA)
          } else {
            return(x$basic_output$observation_results)  
          }
        })
      )
    }
    return(dat)
  }

  if(extract == "culls") {
    return(get_user_data(all_dat=all_dat, type = "culls"))
  }
  
  if(extract == "scares") {
    return(get_user_data(all_dat=all_dat, type = "scares"))
  }

  if(extract == "crops") {
    return(get_user_data(all_dat=all_dat, type = "crops"))
  }
  
  if(extract == "yield") {
    return(get_user_data(all_dat=all_dat, type = "yield"))
  }
  
  if(extract == "budget") {
    return(get_user_data(all_dat=all_dat, type = "budget"))
  }
  
}

### plot_resource()
### 
### Plots actual and/or observed resource pop trends from gmse_apply() simulation results.
### 
### Can plot different types of output - 
### - resource: "true" resource only
### - observation: "observed" resources
### - resobs: both true and observed resources in the same plot.
### For each of the above output types, there are two summary options -
### - mean: plots the mean and upper/lower 95% quantiles across simulations
### - none: plots a line for each simulation
### 
plot_resource = function(gmse_res, type="resources", sumtype="mean", ylim=NULL) {
  
  ### Plot "real" resource number only:
  if(type=="resources") {
    y_res = extract_gmse(gmse_res, "resources")
    manage_target = gmse_res[[1]][[1]]$manage_target
    
    # sumtype == "mean": Summarised as means and upper/lower bounds across simulations:
    if(sumtype == "mean") {
      
      x = 1:ncol(y_res)
      y_mean = apply(y_res, 2, function(x) mean(x, na.rm = T))
      y_lo = apply(y_res, 2, function(x) quantile(x, probs = 0.025, na.rm=T))
      y_hi = apply(y_res, 2, function(x) quantile(x, probs = 0.975, na.rm=T))
      
      if(is.null(ylim)) {
        y_min = bufRange(y_lo, end="lo", incl_val = manage_target )
        y_max = bufRange(y_hi, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type="n", ylim = c(y_min, y_max), xlab = "Time step", ylab = "Resource")
      lines(x = x, y = y_mean, lwd = 2)
      lines(x = x, y = y_lo, lwd = 1, col="grey")
      lines(x = x, y = y_hi, lwd = 1, col="grey")
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
    # sumtype =="none": Not summarised, just each simulation plotted as a line
    if(sumtype == "none") {
      x = 1:ncol(y_res)
      
      if(is.null(ylim)) {
        y_min = bufRange(y_res, end="lo", incl_val = manage_target )
        y_max = bufRange(y_res, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]    
      }
      
      plot(x, y_res[1,], type = "n", ylim=c(y_min, y_max), xlab = "Time step", ylab = "Resource")
      for(i in 1:nrow(y_res)) {
        lines(x, y_res[i,])
      }
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
  }  ## endif type == "resource"

  ### Plot both real and observed resource number:
  if(type=="resobs") {
    
    y_res = extract_gmse(gmse_res, "resources")
    y_obs = extract_gmse(gmse_res, "observations")
    
    manage_target = gmse_res[[1]][[1]]$manage_target
    
    # sumtype == "mean": Summarised as means and upper/lower bounds across simulations:
    if(sumtype == "mean") {
      
      x = 1:ncol(y_res)
      y_res_mean = apply(y_res, 2, mean)
      y_obs_mean = apply(y_obs, 2, mean)
      
      y_res_lo = apply(y_res, 2, function(x) quantile(x, probs = 0.025, na.rm=T))
      y_res_hi = apply(y_res, 2, function(x) quantile(x, probs = 0.975, na.rm=T))
      y_obs_lo = apply(y_obs, 2, function(x) quantile(x, probs = 0.025, na.rm=T))
      y_obs_hi = apply(y_obs, 2, function(x) quantile(x, probs = 0.975, na.rm=T))
      
      if(is.null(ylim)) {
        y_min = bufRange(c(y_res_lo, y_obs_lo), end="lo", incl_val = manage_target )
        y_max = bufRange(c(y_res_hi, y_obs_hi), end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type="n", ylim = c(y_min, y_max), xlab = "Time step", ylab = "Resource")
      lines(x = x, y = y_res_mean, lwd = 2)
      lines(x = x, y = y_obs_mean, lwd = 2, col = "blue")
      
      lines(x = x, y = y_res_lo, lwd = 1, col="darkgrey")
      lines(x = x, y = y_res_hi, lwd = 1, col="darkgrey")
      
      lines(x = x, y = y_obs_lo, lwd = 1, col="blue")
      lines(x = x, y = y_obs_hi, lwd = 1, col="blue")
      
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
    # sumtype =="none": Not summarised, just each simulation plotted as a line
    if(sumtype == "none") {
      x = 1:ncol(y_res)
      
      if(is.null(ylim)) {
        y_min = bufRange(c(y_res,y_obs), end="lo", incl_val = manage_target )
        y_max = bufRange(c(y_res,y_obs), end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_res[1,], type = "n", ylim=c(y_min, y_max), xlab = "Time step", ylab = "Resource")
      for(i in 1:nrow(y_res)) {
        lines(x, y_res[i,], col = "black")
        lines(x, y_obs[i,], col = "blue")
      }
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
  } ## endif type == "resobs"
  
  ### Plot both real and observed resource number:
  if(type=="observations") {
    
    y_obs = extract_gmse(gmse_res, "observations")
    
    manage_target = gmse_res[[1]][[1]]$manage_target
    
    # sumtype == "mean": Summarised as means and upper/lower bounds across simulations:
    if(sumtype == "mean") {
      
      x = 1:ncol(y_obs)
      y_obs_mean = apply(y_obs, 2, function(x) mean(x, na.rm=T))
      
      y_obs_lo = apply(y_obs, 2, function(x) quantile(x, probs = 0.025, na.rm=T))
      y_obs_hi = apply(y_obs, 2, function(x) quantile(x, probs = 0.975, na.rm=T))
      
      if(is.null(ylim)) {
        y_min = bufRange(y_obs_lo, end="lo", incl_val = manage_target )
        y_max = bufRange(y_obs_hi, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_obs[1,], type="n", ylim = c(y_min, y_max), xlab = "Time step", ylab = "Resource (observed)")
      lines(x = x, y = y_obs_mean, lwd = 2, col = "blue")
      
      lines(x = x, y = y_obs_lo, lwd = 1, col="blue")
      lines(x = x, y = y_obs_hi, lwd = 1, col="blue")
      
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
    # sumtype =="none": Not summarised, just each simulation plotted as a line
    if(sumtype == "none") {
      x = 1:ncol(y_obs)
      
      if(is.null(ylim)) {
        y_min = bufRange(y_obs, end="lo", incl_val = manage_target )
        y_max = bufRange(y_obs, end="hi", incl_val = manage_target )
      } else {
        y_min = ylim[1]
        y_max = ylim[2]
      }
      
      plot(x, y_obs[1,], type = "n", ylim=c(y_min, y_max), xlab = "Time step", ylab = "Resource (observed)")
      for(i in 1:nrow(y_obs)) {
        lines(x, y_obs[i,], col = "blue")
      }
      abline(h = manage_target, col="red", lty = "dashed")
    }
    
  } ## endif type == "observation
  
}

### plot_actions()
### 

plot_actions = function(gmse_res, type = "mean", sumtype = "stakeholder") {
  
  no_sims = len(gmse_res)
  no_years = max(unlist(lapply(gmse_res, len)))
  
  culls = get_user_data(gmse_res, "culls")
  scares = get_user_data(gmse_res, "scares")
  crops = get_user_data(gmse_res, "crops")
  
  culls = to.array(culls)
  scares = to.array(scares)
  crops = to.array(crops)
  
  # Summary statistics per stakeholder, across simulations:
  culls_mn = apply(culls, c(1,2), function(x) mean(x, na.rm=T))
  culls_cv = apply(culls, c(1,2), function(x) sd(x, na.rm=T)/mean(x, na.rm=T) )
  
  scares_mn = apply(scares, c(1,2), function(x) mean(x, na.rm=T))
  scares_cv = apply(scares, c(1,2), function(x) sd(x, na.rm=T)/mean(x, na.rm=T) )
  
  crops_mn = apply(crops, c(1,2), function(x) mean(x, na.rm=T))
  crops_cv = apply(crops, c(1,2), function(x) sd(x, na.rm=T)/mean(x, na.rm=T) )
  
  culls_cv[is.na(culls_cv)] = 0
  scares_cv[is.na(scares_cv)] = 0
  crops_cv[is.na(crops_cv)] = 0
  
  if(type == "mean") {
    ylims_mn = c(bufRange(c(culls_mn,scares_mn,crops_mn), end="lo"),
                 bufRange(c(culls_mn,scares_mn,crops_mn), end="hi"))
    plot(1:nrow(culls_mn), culls_mn[,1], type = "n", ylim = ylims_mn,
         xlab = "Time step", ylab = "Mean number of actions per stakeholder")
    # One trace per user:
    for(i in 1:ncol(culls_mn)) {
      lines(culls_mn[,i], col = "darkred")
      lines(scares_mn[,i], col = "blue")
      lines(crops_mn[,i], col = "darkgreen")
    }  
  }
  
  if(type == "cv") {
    ylims_cv = c(bufRange(c(culls_cv,scares_cv,crops_cv), end="lo"),
                 bufRange(c(culls_cv,scares_cv,crops_cv), end="hi"))
    plot(1:nrow(culls_cv), culls_cv[,1], type = "n", ylim = ylims_cv,
         xlab = "Time step", ylab = "CV of actions per stakeholder")
    # One trace per user:
    for(i in 1:ncol(culls_mn)) {
      lines(culls_cv[,i], col = "darkred")
      lines(scares_cv[,i], col = "blue")
      lines(crops_cv[,i], col = "darkgreen")
    }  
  }

}

### plot_yield()
### 

plot_yield = function(gmse_res, type = "all") {
  
  # Extract all yield data from all sims
  all_yield = get_user_data(gmse_res, type = "yield")
  
  n_stakeholders = gmse_res[[1]][[1]]$stakeholders
  
  # Find max and mins and set yrange limits:
  all_max = unlist(lapply(all_yield, max))
  all_min = unlist(lapply(all_yield, min))
  hi = bufRange(all_max, end = "hi")
  lo = bufRange(all_min, end = "lo")
  
  plot(1:nrow(all_yield[[1]]), all_yield[[1]][,1], type = "n", ylim = c(lo,hi))
  
  if (type == "all") {
    
    stakeholder_cols = alpha(brewer.pal(n_stakeholders, "Paired"),0.8)
    
    for(i in 1:n_stakeholders) {
      lapply(all_yield, function(x) lines(1:nrow(x), x[,i], col = stakeholder_cols[i]) )
    }
    
  }
  
  if (type == "stakeholder_mean") {
    
    stakeholder_cols = brewer.pal(n_stakeholders, "Paired")
    
    for(i in 1:n_stakeholders) {
      sholder_i = lapply(all_yield, function(x) x[,i])
      # This gives matrix for stakeholder i: columns are years, rows are simulations:
      sholder_i = list.to.df(sholder_i)
      sholder_i_mean = apply(sholder_i, 2, mean)
      
      lines(1:length(sholder_i_mean), sholder_i_mean, col = stakeholder_cols[i])
      
    }
    
  }
  
}

### Store parameters and output
### 

save_gmse = function(gmse_paras = gmse_paras, gmse_res = res, dir = "out/") {
  
  gmse_paras_out = as.data.frame(gmse_paras)
  gmse_paras_out = cbind(data.frame(years = years, sims = sims), gmse_paras_out)
  
  fname = Sys.time() 
  fname = gsub(":","", fname)
  fname = gsub("-","", fname)
  fname = gsub(" ","", fname)
  
  gmse_paras_out = cbind(data.frame(file_index = fname),gmse_paras_out)
  
  write.csv(gmse_paras_out, sprintf("%sparas_log.csv", dir))
  
  
  
}