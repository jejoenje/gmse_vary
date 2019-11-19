### Summarising functions, plotting etc.
### 
### 

### Initialises simulations following a specific SIM_NAME (a scenario defined by a set of parameters).
### 
### Assumes a strict folder structure for outputs as well as a specific name for input parameters:
### - From the root project folder, simulation code and outputs are in subfolder 'sims'.
### - Within 'sims', file "global_paras.R" contains parameters common to all simulations. 
###     THIS ALSO SETS NUMBERS OF YEARS/SIMS.
### - Within 'sims' each subfolder corresponds to a set of simulations, identified by function parameter SIM_NAME.
### - Within each subfolder SIM_NAME, there are two files (index_SIM_NAME) and (paras_SIM_NAME), and one further 
###     subfolder. The file index_SIM_NAME contains the code for each simulation, and the file paras_SIM_NAME contains 
###     the correspondng parameter values specific to this simulation run. The subfolder 'out' contains further 
###     subfolders with the output for each simulation run, prefixed by a unique date/timestamp to separate different 
###     runs.
### 
### The function does not have explit return values but ASSIGNS outdir, outidx, and outpath to the global environment.

init_sims = function(SIM_NAME) {
  
  source(sprintf("sims/%s/paras_%s.R", SIM_NAME, SIM_NAME))
  
  ### Set output dir
  outdir = sprintf("sims/%s/out/",SIM_NAME)
  
  ### Create output identifier (date/time string)
  outidx = gsub(":", "", gsub(" ", "", gsub("-", "",Sys.time())))
  ### Create outpath
  outpath = sprintf("%s%s_%s/", outdir, outidx, SIM_NAME)
  ### Create output dir
  dir.create(outpath)
  ### Save current parameters to output dir (note Rdata file to separate from Rds sim files)
  save(gmse_paras, file=sprintf("%sparas_%s_%s.Rdata", outpath, outidx, SIM_NAME))
 
  assign("outdir", outdir, env = globalenv())
  assign("outidx", outidx, env = globalenv())
  assign("outpath", outpath, env = globalenv())
  
}

### Random budget sampling functions:
### 


### This draws a sample of budgets where the majority of stakeholders are poorer than a minority 
### (i.e. capital biased "up").
### The scale parameter indicates the relative size of the "high" budget to the manager's budget.
### (i.e. when this is 1, the "richest" stakeholders will only ever be as rich as the manager; when it is 2, 
### the richest stakeholder will be twice as rich as the manager, etc. )
sample_budgets_ManyPoorFewRich = function(nstakeholders = 8, manager_budget = 1000, scale = 1) {
  return(floor(rbeta(nstakeholders, 1, .5)*(manager_budget*scale)))
}


load_sims = function(outdir = NULL, no_files = NULL) {
  sim_files = list.files(outdir)
  para_file = sim_files
  
  sim_files = sim_files[grep(".Rds", sim_files)]
  para_file = para_file[grep(".Rdata", para_file)]
  
  load(paste(outdir, para_file, sep=""))
  
  sim_files = paste(outdir, sim_files, sep="")
  alldat = list()
  
  if(is.null(no_files)) {
    n_load = length(sim_files)
  } else {
    n_load = no_files
  }
  
  for(i in 1:n_load) {
    simdat = readRDS(sim_files[i])
    alldat[[i]] = simdat
    progress(i)
  }
  
  return(list(sims = alldat, para = gmse_paras))
  
}

### Compress sim files
### 
compress_sim_files = function(dir, idx, outname, del=TRUE) {

  dirstring = sprintf("%s%s", dir,idx)
  
  all_files = list.files(dirstring, full.names = T)
  
  sim_files = all_files[grep(".Rds", all_files)]
  
  outstring = sprintf("%s/%s.gz", dirstring, outname)
  
  tar(tarfile = outstring, files = sim_files, compression = "gzip", tar="tar")
  
  if(del == TRUE) {
    file.remove(sim_files)
  }
  
}


trunc_res = function(simres) {

  out = list(AGENTS = simres[["AGENTS"]],
             RESOURCES = simres[["RESOURCES"]],
             OBSERVATION = simres[["OBSERVATION"]],
             COST = simres[["COST"]],
             ACTION = simres[["ACTION"]],
             #LAND = simres[["LAND"]],
             stakeholders = simres[["stakeholders"]],
             basic_output = simres[["basic_output"]])

  return(out)
}


### get_user_acts()
### 
### Extracts a list of matrixes where each list element is a simulation, each row in a matrix is a year,
###  and each column in a matrix is a user. Each value is the desired user parameter (currently: number 
###  of actions taken (cull, scare, tend crops), yield, or budget.)

gmse_sims = function(years = 5 , sims = 5, sim_paras) {

  # Create sim ID string for storing output under:
  sim_idx = gsub(" ", "", gsub(":", "", gsub("-", "", Sys.time())))
  
  # Check if output dir for sim run exists, if not, create it:
  if(!dir.exists(sprintf("out/%s", sim_idx))) {
    dir.create(sprintf("out/%s", sim_idx))
  }
  
  # Save GMSE paras in output folder.
  # gmse_paras$years = years
  # gmse_paras$sims = sims
  # saveRDS(gmse_paras, sprintf("out/%s/gmse_paras.Rds",sim_idx))
   
  res = list()
  
  for(sim in 1:sims) {
    
    res_year = as.list(rep(NA, years))
    
    sim_old <- gmse_apply(get_res = eval(sim_paras$get_res),
                          land_dim_1 = eval(sim_paras$land_dim_1),
                          land_dim_2 = eval(sim_paras$land_dim_2),
                          land_ownership = eval(sim_paras$land_ownership),
                          tend_crops = eval(sim_paras$tend_crops),
                          scaring = eval(sim_paras$scaring),
                          remove_pr = eval(sim_paras$remove_pr),         
                          lambda = eval(sim_paras$lambda),             
                          res_death_K = eval(sim_paras$res_death_K),         
                          RESOURCE_ini = eval(sim_paras$RESOURCE_ini),       
                          manage_target = eval(sim_paras$manage_target),
                          res_death_type = eval(sim_paras$res_death_type),
                          manager_budget = eval(sim_paras$manager_budget), 
                          user_budget = eval(sim_paras$user_budget),
                          public_land = eval(sim_paras$public_land),
                          stakeholders = eval(sim_paras$stakeholders), 
                          res_consume = eval(sim_paras$res_consume),  
                          observe_type = eval(sim_paras$observe_type), 
                          agent_view = eval(sim_paras$agent_view),
                          agent_move = eval(sim_paras$agent_move),
                          converge_crit = eval(sim_paras$converge_crit),
                          ga_mingen = eval(sim_paras$ga_mingen))
    
    for(year in 1:years) {
      
      sim_new = try({gmse_apply(get_res = "Full", old_list = sim_old)}, silent = T)
      
      if(class(sim_new)=="try-error") {
        if(grepl("Extinction", sim_new[1])) {
          print(sprintf("True extinction, skipping to next sim."))
          res_year[year:years] = "Extinction (true)"
          break()
        } else {
          if(grepl("Error in estimate_abundances", sim_new[1])) {
            print(sprintf("Observed extinction, skipping to next sim."))
            res_year[year:years] = "Extinction (observed)"
            break()
          } else {
            print(sprintf("Observed extinction, skipping to next sim."))
            res_year[year:years] = "Extinction (observed, other error)"
            break()
          }
        }
      } else {
        print(sprintf("Sim %d, year %d", sim, year))
        res_year[[year]] = sim_new
        fname = sprintf("out/%s/sim%04d_year%04d.Rds",sim_idx,sim,year)
        saveRDS(sim_new, fname)
        sim_old <- sim_new
        rm(sim_new)
        suppressMessages({gc()})
      }
    }
    fname = sprintf("out/%s/sim%04d.Rds",sim_idx,sim)
    res[[sim]] = res_year
    saveRDS(res_year, fname)
  }

}




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

