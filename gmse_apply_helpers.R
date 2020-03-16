### Summarising functions, plotting etc.
### 
### 

### This is a helper function to run a single gmse_apply call as a first time step, using a given list of input parameters.
### Only intended as a convenience function to ensure simulation scripts can be relatively "clean".
init_sims = function(gmse_paras = gmse_paras) {
  gmse_apply(get_res = gmse_paras$get_res,
             land_dim_1 = gmse_paras$land_dim_1,
             land_dim_2 = gmse_paras$land_dim_2,
             land_ownership = gmse_paras$land_ownership,
             tend_crops = gmse_paras$tend_crops,
             scaring = gmse_paras$scaring,
             remove_pr = gmse_paras$remove_pr,         
             lambda = gmse_paras$lambda,             
             res_death_K = gmse_paras$res_death_K,         
             RESOURCE_ini = gmse_paras$RESOURCE_ini,       
             manage_target = gmse_paras$manage_target,
             res_death_type = gmse_paras$res_death_type,
             manager_budget = gmse_paras$manager_budget, 
             user_budget = gmse_paras$user_budget,
             public_land = gmse_paras$public_land,
             stakeholders = gmse_paras$stakeholders, 
             res_consume = gmse_paras$res_consume,  
             observe_type = gmse_paras$observe_type, 
             agent_view = gmse_paras$agent_view,
             agent_move = gmse_paras$agent_move,
             converge_crit = gmse_paras$converge_crit,
             ga_mingen = gmse_paras$ga_mingen,
             res_movement = gmse_paras$res_movement,
             res_move_type = gmse_paras$res_move_type)
}


### Sets up an output list structure to save the output of a series of simulation time steps. For a series of 
###  simulations, there would be an output like this for each simualtion.
### This is intended as setting up the output for time step 0 or time step 1, as a starting point for a run of time steps.
### This function takes a single input (dat) which is expected to be the output of a single gmse_apply() call.
### It summarises the results in this using extractUser() calls for yield, budget, crops, kills, scares, noact.
### In addition it extract resource population results (both observed and actual, as well as the landscape position
###  of all resources and yield values for each landscape cell). 
### Output is a list with 9 elements. Seven of these (yield, budget, crops, kills, scares, 
###  noact and pop) are dataframes with a new line for each time step, and themselves are outputs of extractUser() 
###  function calls. The last two list elements are matrixes of resource positions in the landscape and yield values in 
###  the landscape (so a single matrix per new time step).
init_sim_out = function(dat) {
  out_i = list(yield = t(as.data.frame(extractUser(dat, "yield"))),
               budget = t(as.data.frame(extractUser(dat, "budget"))),
               crops =  t(as.data.frame(extractUser(dat, "crops"))),
               kills = t(as.data.frame(extractUser(dat, "kills"))),
               scares = t(as.data.frame(extractUser(dat, "scares"))),
               noact =  t(as.data.frame(extractUser(dat, "noact"))),
               pop = t(as.data.frame(c(dat$basic_output$resource_results, 
                                       dat$basic_output$observation_results))),
               res = list(dat$RESOURCES),
               land_yield = list(dat$LAND[,,2])
               #land_owner = list(dat$LAND[,,3])
               )
  
  return(out_i)
}

### This appends the results from a simulation for a given time step (WITHIN A SIMULATION, SO NOT TAKING ACCOUNT 
###  OF DIFFERENCES BETWEEN SIMS) and appends the results to an existing list.
### New year's results (new) should be given as the "full" output of a gmse_apply() run.
### Existing simulation results should be given at the output of either this function, or the output of init_sim_out().
### In any case, 'existing' is expected to be a list with 9 elements. Seven of these (yield, budget, crops, kills, scares, 
###  noact and pop) are dataframes with a new line for each time step, and themselves are outputs of extractUser() 
###  function calls. The last two list elements are matrixes of resource positions in the landscape and yield values in 
###  the landscape (so a single matrix per new time step).
append_output = function(new, existing) {
  
  existing$yield = rbind(existing$yield, t(as.data.frame(extractUser(new, "yield"))))
  existing$budget = rbind(existing$budget, t(as.data.frame(extractUser(new, "budget"))))
  existing$crops = rbind(existing$crops, t(as.data.frame(extractUser(new, "crops"))))
  existing$kills = rbind(existing$kills, t(as.data.frame(extractUser(new, "kills"))))
  existing$scares = rbind(existing$scares, t(as.data.frame(extractUser(new, "scares"))))
  existing$noact = rbind(existing$noact, t(as.data.frame(extractUser(new, "noact"))))
  existing$pop = rbind(existing$pop,  t(as.data.frame(c(new$basic_output$resource_results, 
                                                        new$basic_output$observation_results))))
  existing$res[[ length(existing$res)+1]] = new$RESOURCES
  existing$land_yield[[ length(existing$land_yield) +1 ]] = new$LAND[,,2]
  #existing$land_owner[[ length(existing$land_owner) +1 ]] = new$LAND[,,3]
  
  return(existing)
}

### Some constants:
### 
ACT_NAMES = c("scares","kills","castr","crops","feeds","helps","noact")

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

# init_sims = function(SIM_NAME) {
#   
#   ### Load GLOBAL parameters
#   #source(sprintf("sims/%s/paras_%s.R", SIM_NAME, SIM_NAME))
#   
#   ### Set output dir
#   outdir = sprintf("sims/%s/out/",SIM_NAME)
#   
#   ### Create output identifier (date/time string)
#   outidx = gsub(":", "", gsub(" ", "", gsub("-", "",Sys.time())))
#   ### Create outpath
#   outpath = sprintf("%s%s_%s/", outdir, outidx, SIM_NAME)
#   ### Create output dir
#   dir.create(outpath)
#   ### Save current parameters to output dir (note Rdata file to separate from Rds sim files)
#   save(gmse_paras, file=sprintf("%sparas_%s_%s.Rdata", outpath, outidx, SIM_NAME))
#   
#   assign("outdir", outdir, env = globalenv())
#   assign("outidx", outidx, env = globalenv())
#   assign("outpath", outpath, env = globalenv())
#   assign("para_path", sprintf("%sparas_%s_%s.Rdata", outpath, outidx, SIM_NAME), env = globalenv())
#   
# }

update_paras_from_grid = function(vals, para_list) {
  for(i in 1:len(vals)) {
    
    if(names(vals[i]) %in% names(para_list)) {
      para_list[which(names(para_list) == names(vals[i]))] = as.vector(vals[[i]])   
    } else {
      para_list[[len(para_list)+1]] = as.vector(vals[[i]])
      names(para_list)[len(para_list)] = names(vals)[i]
    }
  }
  return(para_list)
}

init_out = function(s, y, users) {
  
  POP = expand.grid(SIM=c(1:sims), YEAR=c(1:years))
  POP = POP[order(POP[,"SIM"],POP[,"YEAR"]),]
  POP["N"] = NA
  POP["N_OBS"] = NA
  assign("POP", POP, env = globalenv())
  
  USR = expand.grid(usr = c(1:users), YEAR=c(1:years), SIM=c(1:sims))
  USR = subset(USR, select=c("SIM","YEAR","usr"))
  USR["yld"] = NA
  USR["bud"] = NA
  acts = as.data.frame(matrix(NA, nrow=nrow(USR), ncol=7))
  names(acts) = ACT_NAMES
  USR = cbind(USR, acts)
  assign("USR", USR, env = globalenv())
  
  EXT = expand.grid(SIM = c(1:sims))
  EXT["status"] = FALSE
  EXT["year"] = NA
  assign("EXT", EXT, env = globalenv())
  
}

store_dat = function(dat, s, y, type = "normal") {
  
  if(type=="normal") {
    ### Extract and save population data
    POP[which(POP["SIM"]==s & POP["YEAR"]==y),"N"] = dat[["basic_output"]][["resource_results"]]
    POP[which(POP["SIM"]==s & POP["YEAR"]==y),"N_OBS"] = dat[["basic_output"]][["observation_results"]]
    assign("POP", POP, env = globalenv())
    
    ### Extract and save action data per user (looping through each action)
    for(i in 1:len(ACT_NAMES)) {
      n = ACT_NAMES[i]
      USR[which(USR["SIM"]==s & USR["YEAR"]==y),n] = extractUser(dat, n)
    }
    ### Extract and save yield data:
    USR[which(USR["SIM"]==s & USR["YEAR"]==y),"yld"] = extractUser(sim_new, "yield")
    ### Extract and save budget data (NOTE THAT THIS SHOULD BE DONE AFTER "UPDATING" A BUDGET, IF DESIRED:
    USR[which(USR["SIM"]==s & USR["YEAR"]==y),"bud"] = extractUser(sim_new, "budget")
    
    assign("USR", USR, env = globalenv())  
  } else {
    EXT[which(EXT["SIM"]==s),"status"] = TRUE
    EXT[which(EXT["SIM"]==s),"year"] = y
    assign("EXT", EXT, env = globalenv())  
  }
  
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

### Converts given yield to a return for a next budget.
yield_to_return = function(yield, yield_return, type = "direct") {
  
  if(type == "direct") {
    return(yield)
  }
  if(type == "linear") {
    if(is.null(yield_return)) {
      yield_return = 1
      warning("'yield_return' not specified, assuming 1.")
    }
    return(yield*yield_return)
  }
  if(type == "poisson") {
    if(is.null(yield_return)) {
      yield_return = 1
      warning("'yield_return' not specified, assuming 1.")
    }
    return(rpois(length(yield), lambda = yield*yield_return))
  }
  if(type == "beta1") {
    s1 = 5
    s2 = (s1/yield_return)-s1
    # test = rbeta(1000, s1, s2)
    # hist(test); summary(test)
    u_yield_returns = rbeta(length(yield), s1, s2)
    return(yield*u_yield_returns)
  }
  
}

### distribute_land()

### THIS FUNCTION IS INTENDED TO REPLACE set_land()!

### Function to distribute a matrix of landscape cells among s owners.
### Landscape dimensions are controlled by xd (rows) and y (cols).
### Public land is set by public_land. This is a fraction of the total landscape, the remainder of the landscape
###  is divided up between the s stakeholders - these are numbered 2-(s+1) in the output, public land is equal to 1.
### Type sets the type of distribution produced among owners. Currently only two types are supported.
###  "equal" means an even distribution of non-public land between stakeholders.
###  "oneRich" means that a fraction of non-public land, equal to rich_frac, is allocated to a single owner, with the 
###   remainder evenly split between the rest.
### Public land is always placed on the left-hand side of the matrix, and any non-even ("oneRich") type land on the 
###  right of the matrix. Thus, the remaining evenly distributed land is either in between public land a potential 
###  larger landowner, to the right of public land, or to the left of a single large landowner. 

distribute_land = function(xd, yd, s, public_land = 0, type = "equal", rich_frac = NULL) {
  land = matrix(NA, nrow = yd, ncol = xd)
  total_cells = xd*yd
  
  if(public_land > 0) {
    public_cells =  total_cells*public_land
    owned_cells = total_cells-public_cells
    # Allocate public land:
    land[1:public_cells] = 1
    #land = t(land)
  } else {
    public_cells = 0
    owned_cells = total_cells
  }
  
  # Allocate owned cells:
  
  if(type == "equal") {
    stakeholder_cells = rep(owned_cells/s,s)
    stakeholder_series = rep(1:(len(stakeholder_cells))+1, stakeholder_cells)
    
    for(row in 1:xd) {
      for(col in 1:yd) {
        if(is.na(land[row,col])) {
          land[row,col] = stakeholder_series[1]
          stakeholder_series = stakeholder_series[2:length(stakeholder_series)]
        }
      }
    }
    
    land = rcw(land)
    
  }
  
  if(type == "oneRich") {
    
    if(s<2) stop("Distribution type 'oneRich' requires at least two stakeholders!")
    
    if(is.null(rich_frac)) stop("If using type = 'oneRich', need to specify rich_frac (0-1)")
    
    rich_cells = owned_cells*rich_frac
    others_cells = owned_cells-rich_cells
    others_cells = rep(floor(others_cells/(s-1)),s-1)
    # Find any orphaned (leftover) cells:
    leftover = (owned_cells-rich_cells)-sum(others_cells)
    others_cells[1:leftover] = others_cells[1:leftover]+1 
    
    others_cells = rep(2:s, others_cells)
    
    if(public_cells==0) {
      land[1:rich_cells] = 9
      
      for(row in 1:xd) {
        for(col in 1:yd) {
          if(is.na(land[row,col])) {
            land[row,col] = others_cells[1]
            others_cells = others_cells[2:length(others_cells)]
          }
        }
      }
      land = rccw(land)
    }
    
    if(public_cells>0) {
      land = rcw(rcw(land))
      land[1:rich_cells] = 9
      land = rccw(rccw(land))
      
      for(row in 1:xd) {
        for(col in 1:yd) {
          if(is.na(land[row,col])) {
            land[row,col] = others_cells[1]
            others_cells = others_cells[2:length(others_cells)]
          }
        }
      }
      land = rcw(land)
      
    }
    
  } # endif 'type == "oneRich"'
  
  return(land)
  
} # End of function

###
### set_land()
###
### NOW REPLACED BY distribute_land(); ONLY HERE FOR LEGACY REASONS!
set_land = function(land, s, type, hi_frac = NULL, up_frac = NULL, lo_frac = NULL) {
  print("set_land() is deprecated, please use distribute_land() instead!")
  
  pub_land_present = FALSE
  tland = table(land)
  
  # If assuming equal land dist, return what was given:
  if(type == "equal" | is.null(type) ) {
    return(land)
  }
  
  if(type == "oneRich") {
    
    if("1" %in% names(tland)) {
      pub_land_present = TRUE
      common_land = tland[1]
      tland = tland[2:len(tland)]
      #names(tland) = as.character(1:len(tland))
    } 
    
    pland = sum(tland)
    if(is.null(up_frac)) up_frac = hi_frac+(hi_frac*0.07)
    if(is.null(lo_frac)) lo_frac = hi_frac-(hi_frac*0.07)
    
    rich_frac = rgbeta(1, mean = hi_frac, var = hi_frac/2000, min = lo_frac, max = up_frac)
    rich_size = floor(pland*rich_frac)
    
    rest = pland - rich_size
    
    others = rest/(s-1)
    others = floor(others)
    
    all = c(rich_size, rep(others, s-1))
    all[1] = all[1]+(pland-sum(all))
    
    if(pub_land_present == TRUE) {
      all = c(common_land, all)
      all_cells = as.vector(NULL)
      for(i in 1:len(all)) {
        all_cells = c(all_cells, rep(i, all[i]))
      }
    } else {
      all_cells = as.vector(NULL)
      for(i in 1:len(all)) {
        all_cells = c(all_cells, rep(i+1, all[i]))
      }  
    }
    
    land_out = matrix(all_cells, nrow = nrow(land), ncol = ncol(land))
    
    return(land_out)
  }
}


### set_budgets()
### Takes AGENTS data frame as argument and returns a series of new budgets based on 
### 
### prv = current gmse_apply() list output [ONLY USED IN DEPRECATED PARAMTERISATION]
### nxt = current gmse_apply() list output [ONLY USED IN DEPRECATED PARAMTERISATION]
### yv = yield return value (to be passed to yield_to_return())
### yield_type = yield return type (to be passed to yield_to_return())
### cur = current gmse_apply() list output
### type = function type flag. Original = pre-2020 parameterisation. 2020 is newer one.
### scale = flag to "scale" the return (as a function of budget) by the budget in the previous time step. 
###  PROBABLY REQUIRED when not using "tax" type of reduction in profit. Defaults to TRUE for legacy purposes.
### 
set_budgets = function(prv = NULL, nxt = NULL, yv, yield_type = "beta1", cur = NULL, type = "original", scale = TRUE) {
  
  if(type == "original") {
    # THIS IS THE PARAMETERISATION AS USED PRE-2020.
    rem = extractUser(prv, "budget")-extractUser(nxt, "expenses")
    carryover = rem + extractUser(nxt, "leftover")
    curr_yield = extractUser(nxt, "yield")
    profit = yield_to_return(yield = curr_yield, yield_return = yv, type = yield_type)
    return(carryover+profit)
  }
  
  if(type == "2020") {
    
    if(is.null(cur)) stop("For '2020' parameterisation type, need to explicitly specify 'cur'." )
    
    ### Set next time step's budget
    # Extract yield, current budget, and expenses:
    y = extractUser(cur, "yield")
    b = extractUser(cur, "budget")
    e = extractUser(cur, "expenses")
    # Calculate yield return according to input paras 
    r = yield_to_return(y, type = yield_type, yield_return = yv)
    # "Scale" the return (as a function of budget) by the budget in the previous time step.
    if(scale == TRUE) {
      r = b+(((r-mean(r))/r)*b)
    }
    # New USER budget is old budget, minus expenses, plus scaled return:
    new_b = b-e+r
    
    return(new_b)
  }
    
}

#' Plot a "land" matrix as produced by GMSE (LAND[,,3]) as coloured image.
#' 
#' @param x A matrix of whole integer numbers where each number represents a land "owner".
#' @param col A \code{\link{RColorBrewer}} color set code to indicate the colors to use. 
#' @return Nothing, plots an image.
#' @examples
#' plot_land(sim$LAND[,,3])
plot_land = function(x, col = "BrBG") {
  
  # Pick colors
  land_cols = brewer.pal(len(table(x))+2, col)
  land_cols = land_cols[!land_cols=="#F5F5F5"]

  if(sum(x == 1, na.rm = T)>0) land_cols[1] = "#FFFFFF"
  image(x = x, col = land_cols, yaxt = "n", xaxt = "n")
 
}


### Set manager budget according to user budgets
set_man_budget = function(u_buds, type = "max", p = NULL, fixed_budget = 1000) {
  allowed_types = c("fixed", "max","mean","prop","0.75","0.9","0.99")
  
  if(!(type %in% allowed_types)) {
    stop(sprintf("Type '%s' not implemented, please choose from: %s", type, paste(allowed_types, collapse=", ")))
  }
  
  if(type == "prop" & is.null(p)) {
    stop("If type = prop, need to specify p.")
  }
  
  if(type == "max") {
    return(max(u_buds))
  }
  if(type == "mean") {
    return(mean(u_buds))
  }
  if(type == "prop") {
    return(sum(u_buds*p))
  }
  if(type == "0.75") {
    return(as.numeric(quantile(u_buds, probs = c(0.75))))
  }
  if(type == "0.9") {
    return(as.numeric(quantile(u_buds, probs = c(0.9))))
  }
  if(type == "0.99") {
    return(as.numeric(quantile(u_buds, probs = c(0.99))))
  }
  if(type == "fixed") {
    return(fixed_budget)
  }
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
             manager_array = simres[["manager_array"]],
             #LAND = simres[["LAND"]],
             minimum_cost = simres[["minimum_cost"]],
             stakeholders = simres[["stakeholders"]],
             basic_output = simres[["basic_output"]])

  return(out)
}


### Takes a single $LAND list and calculates PRODUCTION for each user
calcProd = function(land) {
  
  # Enumerate all users in input:
  all_users = names(table(land[,,3]))
  
  production = as.vector(NULL)
  for(i in 1:len(all_users)) {
    production = c(production, sum(land[,,3][which(land[,,3]==all_users[i])]))
  }
  return(production)
}


### Takes $LAND, $RESOURCE and $res_consume and calculates YIELD, which is production
###  minus damage by resource
calcProd_damage = function(res, land, res_consume) {
  
  res_count = matrix(0, nrow = nrow(land[,,3]), ncol = ncol(land[,,3]) )
  
  for(i in 1:nrow(res)) {
    res_count[res[i,5],res[i,6]] = res_count[res[i,5],res[i,6]]+1
  }
  ### Update production with damage by resource:
  land[,,2] = land[,,2]*(gmse_paras$res_consume^res_count)
  
  ### Calculate reduced production:
  return(calcProd(land))
  
}




###
### extractUser() does the same as get_user_data but on a single gmse_apply() list output (single sim/year)
extractUser = function(dat, type) {
  
  # Get number of stakeholders
  n_stakeholders = dat$stakeholders
  # Get all AGENTS data for users only (exlude manager)
  agents = dat[["AGENTS"]][2:nrow(dat[["AGENTS"]]),]
  # Get all ACTION data for users only (exclude manager)
  actions = dat[["ACTION"]][,,2:(n_stakeholders+1)]

  # Get costs set per user.
  # Not used currently:
  costs = dat[["manager_array"]][,,2:(n_stakeholders+1)]

  if(type == "yield") {
    return(agents[,16])
  }
  
  if(type == "budget") {
    return(agents[,17])
  }
  
  if(type == "crops") {
    return(apply(actions, 3, function(x) x[2,10]))
  }
  
  if(type == "kills") {
    return(apply(actions, 3, function(x) x[1,9]))
  }
  
  if(type == "scares") {
    return(apply(actions, 3, function(x) x[1,8]))
  }
  
  if(type == "castr") {
    return(apply(actions, 3, function(x) x[1,10]))
  }
  
  if(type == "feeds") {
    return(apply(actions, 3, function(x) x[1,11]))
  }
  
  if(type == "helps") {
    return(apply(actions, 3, function(x) x[1,12]))
  }
  
  if(type == "noact") {
    return(apply(actions, 3, function(x) x[1,13]))
  }
  
  ### 'all_actions' returns the total number of actions per user.
  if(type == "all_actions") {
    all_act = apply(actions, 3, function(x) {
        x[1,8]+   # SCARING
        x[1,9]+   # CULLING
        x[1,10]+  # CASTRATION
        x[2,10]+  # CROPPING
        x[1,11]+  # FEEDING
        x[1,12]+  # HELPING
        x[1,13]   # NOTHING
      })
    return(all_act)
  }

  ### Returns "expenses"  
  if(type == "expenses") {
    
    # Set costs > 100000 (i.e. actions not allowed) to 0 so we can calculate with it.
    for(i in 1:dim(costs)[3]) {
      costs[,,i][costs[,,i]>100000] = 0
    }

    expenses = costs*actions
    expenses = expenses[,8:12,]
    return(apply(expenses, 3, function(x) sum(x, na.rm=T)))
  }
  
  ### 'all_costs' returns total costs per user.
  ### THIS EXCLUDES "NOTHING" AS IT ASSUMES IT "COSTS" NOTHING.
  if(type == "all_costs") {
    all_act = apply(actions, 3, function(x) {
      x[1,8]+   # SCARING
        x[1,9]+   # CULLING
        x[1,10]+  # CASTRATION
        x[2,10]+  # CROPPING
        x[1,11]+  # FEEDING
        x[1,12]  # HELPING
    })
    return(all_act)
  }
  
  # Get "leftover" budget. Should usually be zero and == "nothing" actions times minimum cost.
  if(type == "leftover") {
    nothing_acts = apply(actions, 3, function(x) x[1,13])
    return(nothing_acts*gmse_paras[["minimum_cost"]])
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


### plot_budgets()
### 

plot_budgets = function(gmse_res) {
  
  all_budgets = get_user_data(gmse_res, "budget")
  n_stakeholders = gmse_res[[1]][[1]]$stakeholders
  
  # Find max and mins and set yrange limits:
  all_max = unlist(lapply(all_budgets , max))
  all_min = unlist(lapply(all_budgets , min))
  hi = bufRange(all_max, end = "hi")
  lo = bufRange(all_min, end = "lo")
 
  plot(1:nrow(all_budgets[[1]]), all_budgets[[1]][,1], type = "n", ylim = c(lo,hi))
  
  stakeholder_cols = alpha(brewer.pal(n_stakeholders, "Paired"),0.8)
  
  for(i in 1:n_stakeholders) {
    lapply(all_budgets, function(x) lines(1:nrow(x), x[,i], col = stakeholder_cols[i]) )
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


###
### plot_gmse_sims()
###
### This function is intended to plot the results from "new style", "flat-file" gmse_apply sim outputs.


plot_gmse_sims = function(pop, usr=NULL) {
  
  # Reconstruct number of sims and number of years in output:
  s = max(pop$SIM)
  s_years = tapply(pop$YEAR, pop$SIM, max)
  y = max(s_years)
  
  if(!is.null(usr)) {
    # Reconstruct no. of stakeholders
    stakeholders = max(as.numeric(names(table(usr$usr))))
  }
  
  if(!is.null(usr)) {
    # Set up plots
    par(mfrow=c(2,2))
  } else {
    par(mfrow=c(1,1))
  }
  
  if(is.null(usr)) {
    # 1. Resources
    y_max = bufRange(pop[,"N"], end="hi",incl_val = gmse_paras$manage_target)
    y_min = bufRange(pop[,"N"], end="lo",incl_val = gmse_paras$manage_target)
    plot(pop$YEAR, pop$N, type="n", ylab = "Resource population", xlab = "Time step", 
         ylim=c(y_min,y_max), xlim = c(0, max(pop$YEAR)))
    for(i in 1:s) {
      lines(pop[pop["SIM"]==i,"YEAR"], pop[pop["SIM"]==i,"N"])
    }
    abline(h = gmse_paras$manage_target, col  = "red", lty = "dashed")
    
  } else {
    
    # 1. Resources
    y_max = bufRange(pop[,"N"], end="hi",incl_val = gmse_paras$manage_target)
    y_min = bufRange(pop[,"N"], end="lo",incl_val = gmse_paras$manage_target)
    plot(pop$YEAR, pop$N, type="n", ylab = "Resource population", xlab = "Time step", 
         ylim=c(y_min,y_max), xlim = c(0, max(pop$YEAR)))
    for(i in 1:s) {
      lines(pop[pop["SIM"]==i,"YEAR"], pop[pop["SIM"]==i,"N"])
    }
    abline(h = gmse_paras$manage_target, col  = "red", lty = "dashed")
    
    # 2. Actions
    curacts = c("scares","kills","crops","noact")
    pltact = subset(usr, select = c("SIM","YEAR","usr", curacts))
    y_max = bufRange(pltact[,curacts], end="hi", buffer = 0.075)
    y_min = bufRange(pltact[,curacts], end="lo")
    # Calculate mean actions across simulations for each user (row) per year (column)
    kills_mn = tapply(pltact$kills,list(pltact$usr,pltact$YEAR), function(x) mean(x, na.rm=T))
    crops_mn = tapply(pltact$crops,list(pltact$usr,pltact$YEAR), function(x) mean(x, na.rm=T))
    scares_mn = tapply(pltact$scares,list(pltact$usr,pltact$YEAR), function(x) mean(x, na.rm=T))
    noact_mn = tapply(pltact$noact,list(pltact$usr,pltact$YEAR), function(x) mean(x, na.rm=T))
    
    act_cols = brewer.pal(5, "PRGn")
    act_cols = act_cols[c(1,2,5)]
    alph = 0.8
    
    plot(pltact$YEAR,pltact$kills,type="n", ylim=c(y_min,y_max), 
         xlab = "Time step", ylab = "Mean stakeholder actions", xlim = c(0, max(pltact$YEAR)))
    for(i in 1:stakeholders) {
      lines(1:y,kills_mn[i,], col = alpha(act_cols[1],alph))
      lines(1:y,scares_mn[i,], col = alpha(act_cols[2],alph))
      lines(1:y,crops_mn[i,], col = alpha(act_cols[3],alph))
    }
    
    legend(x = 1, y = y_max*1.05, legend = c("Culling","Scaring","Farming"), 
           fill = act_cols, horiz = T, bty = "n", x.intersp = 0.1, text.width = c(1,1,1))
    
    
    # 3. Yields
    
    y_range = c(bufRange(usr$yld, end = "lo"),bufRange(usr$yld, end = "hi") ) 
    plot(usr$YEAR,usr$yld, type="n", xlab = "Time step", ylab = "Yield per user", 
         ylim = y_range,xlim = c(0, max(usr$YEAR)))
    
    stakeholder_cols = alpha(brewer.pal(stakeholders, "Paired"),0.8)
    
    for(u in 1:stakeholders) {
      u_col = stakeholder_cols[u]
      
      for(sim in 1:s) {
        set = usr[(usr$usr == u & usr$SIM == sim),]
        lines(set$YEAR,set$yld, col = u_col)
      }
      
    }
    
    # 4. Budgets
    y_range = c(bufRange(usr$bud, end = "lo"),bufRange(usr$bud, end = "hi") ) 
    plot(usr$YEAR,usr$bud, type="n", xlab = "Time step", ylab = "Budget per user", ylim = y_range, xlim = c(0, max(usr$YEAR)))
    stakeholder_cols = alpha(brewer.pal(stakeholders, "Paired"),0.8)
    for(u in 1:stakeholders) {
      u_col = stakeholder_cols[u]
      for(sim in 1:s) {
        set = usr[(usr$usr == u & usr$SIM == sim),]
        lines(set$YEAR,set$bud, col = u_col)
      }
    }
    
  }
    
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

# This function creates a square buffer with side == buffer, centered on x,y point in a given matrix land.
# Returns a matrix of the same size as land, with values for TRUE if within given buffer of the x,y point.
# "Wraps" buffer on the edge of the matrix to appear on the mirroring side.
#
land_point_buffer = function(x,y,land,buffer,type = "matrix") {
  
  # Extract landscape dimensions (mins/maxs)
  xd = dim(land)[1]
  yd = dim(land)[2]
  
  xr = (x-buffer):(x+buffer) # X range given coordinate and buffer
  yr = (y-buffer):(y+buffer) # Y range given coordinate and buffer
  
  # "Wrap" X/Y range as needed if coordinates outwidth min/max dimensions.
  yr[yr>yd] = yr[yr>yd]-yd                 
  yr[yr<1] = yd+yr[yr<1]
  xr[xr>xd] = xr[xr>xd]-xd  
  xr[xr<1] = xd+xr[xr<1]
  
  # Create "empty" output mask:
  land_mask = matrix(FALSE, nrow = xd, ncol = yd)
  
  # Set output cells to "true" if for those within boundaries:
  land_mask[xr, yr] = TRUE
  
  if(type == "matrix") {
    # Return landscape mask:
    return(land_mask)  
  }
  
  if(type == "xy") {
    return(list(x=xr, y = yr))
  }
  
}

# Function takes landscape yield matrix and a matrix of the same size with TRUE/FALSE values for which cells are 
#  within spatial reaching distance (as returned by land_point_buffer(.., type = "matrix)), and returns a list with 
#  new x and y coordinates to which a given resource (as defined by the the input/output of land_point_buffer() ) will 
#  move. This is the result of a random pick of one of the cells within mask, which is also greater than a given quantile 
#  value of the distribution of yield within that mask. 
res_move_adjusted = function(mask, yield, type = "qtl", qtl = 0.975) {
  
  type_check = FALSE
  if(type == "qtl") {
    # Take existing mask (spatial range of point) and further limit by what is above a given qtl of yield:
    mask_extra = (yield >= quantile(yield[mask], qtl)) & mask
    type_check = TRUE
  }
  if(type == "max") {
    mask_extra = (yield == max(yield[mask]) & mask)
    type_check = TRUE
  }
  if(type_check==FALSE) { stop("Invalid 'type' argument, please choose 'qtl' or 'max'") }
  
  # Get XY positions for the above:
  xy_picks = which(mask_extra, arr.ind = T)
  
  # Pick one new position at random:
  xy_pick = xy_picks[sample(1:nrow(xy_picks),1),]
  
  # Return new X/Y location
  return(list(x = as.numeric(xy_pick["row"]), y = as.numeric(xy_pick["col"])))
}


### This function takes a GMSE RESOURCE array and the landscape dimensions, and places each individual resource
###  into a matrix which can be directly compared the the relevant GMSE LAND array.
###
### NOTE - DUE TO INCREMENTING DIFFERENCES BETWEEN R and C, THE X AND Y COORDINATES IN THE RESOURCE ARRAY
###  START FROM ZERO (AND GO UP TO xd-1, yd-1). HENCE THE VALUES RETURNED (AND THUS WHERE RESOURCES ARE PLACED)
###  IN THE OUTPIT MATRIX ARE +1!
placeResources = function(res, xd, yd) {
  land_res = matrix(0, nrow = xd, ncol = yd)
  for(i in 1:nrow(res)) {
    land_res[res[i,5]+1,res[i,6]+1] = land_res[res[i,5]+1,res[i,6]+1]+1
  }
  land_res[land_res==0] = NA
  
  return(land_res)
}

### This function takes a resource position matrix (as output from placeResources() and returns a table with a count of
###  (non-zero) landscape positions, and the resulting mean yield in that cell.
### This is largely for testing purposes alone (relating yield to resource positions)
yield_res_rel = function(res_pos, yld) {
  res_pos[is.na(res_pos)] = 0
  no_per_cell = as.numeric(names(table(res_pos)))
  mn_yld = as.vector(NULL)
  for(i in 1:length(no_per_cell)) {
    mn_yld = c(mn_yld, mean(yld[res_pos == no_per_cell[i]]))
  }
  return(data.frame(no_per_cell = no_per_cell, mn_yd = mn_yld))
}

# This function will take GMSE RESOURCES array, and for each row (ie resource), run land_point_buffer() and 
#  res_move_adjusted() functions; i.e. establish movement range and then pick a new position within range that
#  also has a relatively high yield.
move_resources_adjusted = function(res, land, buffer, type = "qtl", qtl = 0.975) {
  
  new_positions = as.data.frame(NULL)
  for(i in 1:nrow(res)) {
    # Extract X and Y coordinates for resource. Note need to +1 to adjust for 0-base x/y coordinates in RESOURCES
    res_x = res[i,5]+1
    res_y = res[i,6]+1
    
    res_buffer = land_point_buffer(x = res_x, y = res_y, land = land, buffer = buffer)
    
    new_xy = res_move_adjusted(mask= res_buffer, yield = land, type = type, qtl = qtl)
    
    new_positions = rbind(new_positions, unlist(new_xy))
    
  }
  
  # Note - need to "back translate" to coordinates with base 0
  new_positions = new_positions-1
  return(as.matrix(new_positions))
  
}

### This is a simple wrapper for a call to move_resources_adjusted() which does some error
###  checking for weid parameter combinations
move_res = function(old = sim_old, paras = gmse_paras) {
  
  ### Move resources according to yield, if needed:
  if(paras$res_move_to_yield==TRUE && paras$res_move_type==0 ) {
    old$RESOURCES[,5:6] = move_resources_adjusted(old$RESOURCES, old$LAND[,,2], 
                                                      buffer = paras$res_movement, 
                                                      type = "max")  
  }
  if(paras$res_move_to_yield==TRUE && paras$res_move_type!=0) {
    stop("Invalid res_move_to_yield / res_move_type combination")
  }
  if(paras$res_move_to_yield==FALSE && paras$res_move_type==0) {
    stop("res_move_type set to 0 (no movement) but res_move_to_yield also FALSE - static resources!")
  }
  
  return(old)
  
}

### This is a wrapper to do some error checks of the state (class) of a recent gmse_apply() run, to make
###  sure no extinctions occurred. 
### Basically, function checks class of sim_new == "try-error". If so, attempts to identify why and print a message.
###  Then returns a text string to indicate extinction.
### If not, return "ok" string.
### 
### NOTE THIS ASSUMES THAT ANY "BREAKING" OUT OF A LOOP OCCURS OUTWITH THIS FUNCTION!
check_gmse_extinction = function(new = sim_new, silent = FALSE) {
  
  if(class(new)=="try-error") {
    if(grepl("Extinction", sim_new[1])) {
      if(silent == FALSE) print(sprintf("True extinction, skipping to next sim."))
    } else {
      if(grepl("Error in estimate_abundances", sim_new[1])) {
        if(silent == FALSE) print(sprintf("Observed extinction, skipping to next sim."))
      } else {
        if(silent == FALSE) print(sprintf("Observed extinction, skipping to next sim."))
      }
    }
    return("extinct")
  } else {
    return("ok")
  }
}

### Reads all .rds files in given folder, assuming these are outputs from gmse_apply() simulation runs as 
###  created by init_sims(), init_sim_out() and append_output().
### Loads each output file and combines into a single data file.

gmse_rds_summary = function(folder) {
  
  if(substr(folder, nchar(folder), nchar(folder)) != "/" ) {
    folder = paste0(folder, "/")
  }
  
  file_list = list.files(folder)
  is_rds = grepl(".Rds", list.files(folder))
  rds_files = file_list[is_rds]
  
  all_dat = list()
  for(i in 1:length(rds_files)) {
    all_dat[[i]] = readRDS(paste0(folder, rds_files[i]))
  }
  
  return(all_dat)
}

### This function plots summaries from a list of gmse_apply() output series.
### Each list element of 'dat' is expected to be the output of a simulation run of years, consisting of ten list elements 
###  themselves: yield, budget, crops, kills, scares, noact, pop, res, land_yield and par, the first nine of which
###  lists the results of each of these for each simulation year.
### type = "pop" or "yield". Plots population trajectories for each simulation, or mean yield across users against year, 
###  for each simulation. Remaining parameters are plotting controls passed on to low-level plotting functions.
### For type == yield and type == budget, parameter sumtype is necessary, which defaults to mean, plotting the
###  mean across users. Can also be "min", "max", "median".
gmse_vary_plot = function(dat, type = "pop", col = "black", lwd = 1, ylim = NULL, xlim = NULL, 
                          sumtype = "mean", ylab = "", xlab = "") {
  
  if(type == "pop") {
    # Population trajectories, one for each sim:
    y_lo = min(unlist(lapply(dat, function(x) min(x$pop[,1]))))
    y_hi = max(unlist(lapply(dat, function(x) max(x$pop[,1]))))
    plot(dat[[1]]$pop[,1], type = "n", ylim = c(y_lo,y_hi), xlim = c(0,dat[[1]]$par$n_years)+1, 
         ylab = ylab, xlab = xlab)
    lapply(dat, function(x) lines(x$pop[,1], col = col, lwd = 1))
  }
  
  if(type == "yield") {
    # Average yield across users, per year.
    y_lo = min(unlist(lapply(dat, function(x) min(x$yield))))
    y_hi = max(unlist(lapply(dat, function(x) max(x$yield))))
    plot(dat[[1]]$yield[,1], type = "n", ylim = c(y_lo,y_hi), xlim = c(0,dat[[1]]$par$n_years)+1, 
         ylab = ylab, xlab = xlab)
    lapply(dat, function(x) lines(apply(x$yield,1,sumtype), col = col, lwd = 1))
  }
  
  if(type == "budget") {
    y_lo = unlist(lapply(dat, function(x) min(x$budget)))
    y_hi = unlist(lapply(dat, function(x) max(x$budget)))
    y_lo = bufRange(y_lo, end = "lo")
    y_hi = bufRange(y_hi, end = "hi")
    plot(dat[[1]]$budget[,1], type = "n", ylim = c(y_lo,y_hi), xlim = c(0,dat[[1]]$par$n_years)+1,
         ylab = ylab, xlab = xlab)
    lapply(dat, function(x) lines(apply(x$budget,1,sumtype), col = col, lwd = 1))
  }
  
}
