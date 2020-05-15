### Cleaning up gmse_apply_helpers.R

### Helper function to check, set and empty simulation output folder.
### THIS IS DEFUNCT
# checkOutFolder = function(foldername) {
#   accept = c("Yes","yes","Y","y")
#   folder = paste0("sims/",foldername,"/")
#   if(!dir.exists(folder)) { 
#     dir.create(folder)
#     print("Created new output folder.")
#     return(folder)
#   } else {
#     checkOut = readline(prompt="Output folder already exists. Continue with new datestamped folder? (Y/Yes/yes/y)")
#     if( !(checkOut %in% accept) ) {
#       stop("Output folder already exists, aborting.")
#     } else {
#       time_stamp = format(Sys.time(), "%Y%m%d%H%M%OS6")
#       time_stamp = gsub("\\.", "", as.character(time_stamp))
#       folder_new = paste0("sims/",foldername,"_",time_stamp,"/")
#       dir.create(folder_new)
#       return(folder_new)
#     }
#   }
# }


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

### Set manager budget according to user budgets
###### DIRECT COPY FROM gmse_apply_helpers.R ON 7/05/2020
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


###
### extractUser() does the same as get_user_data but on a single gmse_apply() list output (single sim/year)
###### DIRECT COPY FROM gmse_apply_helpers.R ON 7/05/2020
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

### set_budgets()
###
### COPY FROM gmse_apply_helpers.R ON 7/05/2020 BUT WITH OLD PARAMETERISATION REMOVED
###
### Takes AGENTS data frame as argument and returns a series of new budgets based on 
### 
### yv = yield return value (to be passed to yield_to_return())
### yield_type = yield return type (to be passed to yield_to_return())
### cur = current gmse_apply() list output

set_budgets = function(cur = NULL, yv, yield_type = "linear") {

  if(is.null(cur)) stop("need to explicitly specify 'cur'." )
  
  ### Set next time step's budget
  # Extract yield, current budget, and expenses:
  y = extractUser(cur, "yield")
  b = extractUser(cur, "budget")
  e = extractUser(cur, "expenses")
  # Calculate yield return according to input paras 
  r = yield_to_return(y, type = yield_type, yield_return = yv)

  # New USER budget is old budget, minus expenses, plus return:
  new_b = b-e+r
  
  return(new_b)

}

### Converts given yield to a return for a next budget.
###### DIRECT COPY FROM gmse_apply_helpers.R ON 07/05/2020
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


### This is an alternative to distribute_land(), using a simplified process.
### The only parameter is land_var, a proportion which determines the ratio of the standard 
###  deviation to the mean, of a truncated normal distribution form which each stakeholder's 
###  land holding is drawn. For example, a land_var of 0.1 means that the standard deviation of 
###  each random land draw is 0.1 times its mean (i.e. an equal proportion of the total land 
###  remaining). Thus, land_var needs to be >0 and <1, and values approaching 0 mean a more or 
###  less uniform land distribution.
### Currently, public land is not supported.
distribute_land_simplified = function(xd, yd, s, land_var = NULL) {
  
  require(truncnorm)
  
  if(is.null(land_var)) land_var = 0.0000001
  if(land_var<0 | land_var>0.999999) stop("land_var needs to be >0 and <1.")
  
  landf = as.vector(NULL)
  remaining_size = xd*yd
  S = s
  
  for(i in 1:s) {
    equal = (1/S)*remaining_size
    min_size = equal-equal*land_var
    max_size = remaining_size-(equal+equal*land_var)
    pick_size = rtruncnorm(1, a = min_size, b = max_size, mean = equal, sd = equal*land_var )
    landf = c(landf, pick_size)
    remaining_size = remaining_size - pick_size
    S = S-1
    
  }
  remaining_size = (xd*yd)-sum(landf,na.rm=T)
  
  if(remaining_size>0) {
    if(len(landf[is.na(landf)])>1) {
      split_remaining = rtruncnorm(1, mean = remaining_size/2, 
                                   sd = (remaining_size/2)*land_var, 
                                   a = land_var*(remaining_size/2), 
                                   b = remaining_size-(land_var*(remaining_size/2)) )
      landf[is.na(landf)]
      landf[is.na(landf)] = c(split_remaining,(remaining_size-split_remaining ))
    } else {
      landf[is.na(landf)] = remaining_size
    }
  }
  
  landf = floor(landf)
  
  missing = (xd*yd)-sum(landf)
  pad_out = sample(1:s,missing,replace=T)
  for(i in 1:length(pad_out)) {
    landf[pad_out[i]] = landf[pad_out[i]] + sign(missing)
  }
  #sum(landf)
  
  land = rep(1:s, landf)
  land = matrix(land, xd, yd)
  
  return(land)
}



### Print an output string to screen, for testing purposes only.
print_sim_status = function(i, sim) {
  pop = sim$basic_output$resource_results
  y = round(extractUser(sim, "yield"),1)
  b = round(extractUser(sim, "budget"),1)
  return(cat("t=",i,"\t","pop=",pop,"\t",y,"\t",b,"\n"))
}

### Function used to plot a list of gmse_apply output
###
### 'sims' is expected to be a list of K simulations, each in turn consisting of a list of 
###  gmse_apply() output objects.
### sim_act is only used for Action plotting; if NULL a random sim series is picked to plot
###  action sums per time step.
plot_gmse_apply = function(dat, displaytype = "pop", manage_target = NULL, n_yrs = NULL,
                           act_sim = NULL, act_type = "abs", pop_col = NULL) {
  
  if(is.null(n_yrs)) stop("Need to specify max n_yrs to plot")
  
  n_sims = length(dat)

  # Realised no. years per sim
  n_yrs_per_sim = unlist(lapply(dat, length))

  s = dat[[1]][[1]]$stakeholders
    
  if(displaytype == "pop") {
    
    # Extract all population data from gmse_apply() lists:
    pop = lapply(dat, function(x) lapply(x, function(y) y$basic_output$resource_results))
    
    pop_mat = matrix(NA, nrow = n_yrs, ncol = length(pop))
    for(i in 1:length(pop)) {
      pop_i = unlist(pop[[i]])
      pop_mat[,i] = c(pop_i, rep(NA, n_yrs-length(pop_i)))
    }
    pop = pop_mat

    plot(pop[,1], type = "n", ylim = c(0,max(pop,na.rm=T)*1.1), 
         xlab = "Time step", 
         ylab = "Population size")
    apply(pop, 2, function(x) lines(x) )
    if(!is.null(manage_target)) abline(h = manage_target, col = "red", lty="dashed")
    if(!is.null(pop_col)) lines(pop[,pop_col], col = "red", lwd = 1.5)
  }
  
  if(displaytype == "mean_yields") {
    
    ### Extract all YIELD data from gmse_apply() lists.

    yield = lapply(dat, function(x) lapply(x, function(y) extractUser(y, "yield")))
    yield_u = lapply(yield, list.to.df)
    
    mean_yield_u = matrix(NA, nrow = s, ncol = n_yrs)
    for(i in 1:s) {
      s_i = lapply(yield_u, function(x) x[,i])
      s_i = lapply(s_i, function(x) c(x, rep(NA, n_yrs-length(x))) )
      s_i = list.to.df(s_i)
      mean_yield_u[i,] = as.vector(apply(s_i, 2, function(x) mean(x, na.rm=T)))
    }
    mean_yield_u[mean_yield_u=="NaN"]=NA
    
    plot(mean_yield_u[1,], type = "n", ylim = c(0,max(mean_yield_u, na.rm =T)*1.1), 
         xlab = "Time step", 
         ylab = "Yield (average per user across simulations)")
    apply(mean_yield_u, 1, function(x) lines(x, col = "darkgreen") )
    
  }
  
  if(displaytype == "mean_budgets") {
    
    ### Extract all BUDGET data from gmse_apply() lists.
    
    budget = lapply(dat, function(x) lapply(x, function(y) extractUser(y, "budget")))
    budget_u = lapply(budget, list.to.df)
    
    mean_budget_u = matrix(NA, nrow = s, ncol = n_yrs)
    for(i in 1:s) {
      s_i = lapply(budget_u, function(x) x[,i])
      s_i = lapply(s_i, function(x) c(x, rep(NA, n_yrs-length(x))) )
      s_i = list.to.df(s_i)
      mean_budget_u[i,] = as.vector(apply(s_i, 2, function(x) mean(x, na.rm=T)))
    }
    mean_budget_u[mean_budget_u=="NaN"]=NA
  
    plot(mean_budget_u[1,], type = "n", ylim = c(0,max(mean_budget_u, na.rm =T)*1.1), 
         xlab = "Time step", 
         ylab = "Budget (average per user across simulations)")
    apply(mean_budget_u, 1, function(x) lines(x, col = "blue") )
  }
  
  if(displaytype == "actions") {
    
    kills = lapply(dat, function(x) lapply(x, function(y) sum(extractUser(y,"kills")) ))
    scares = lapply(dat, function(x) lapply(x, function(y) sum(extractUser(y,"scares")) ))
    crops = lapply(dat, function(x) lapply(x, function(y) sum(extractUser(y,"crops")) ))
    
    kills_mat = matrix(NA, nrow = n_yrs, ncol = length(kills))
    scares_mat = matrix(NA, nrow = n_yrs, ncol = length(scares))
    crops_mat = matrix(NA, nrow = n_yrs, ncol = length(crops))
    for(i in 1:length(kills)) {
      kills_i = unlist(kills[[i]])
      scares_i = unlist(scares[[i]])
      crops_i = unlist(crops[[i]])
      kills_mat[,i] = c(kills_i, rep(NA, n_yrs-length(kills_i)))
      scares_mat[,i] = c(scares_i, rep(NA, n_yrs-length(scares_i)))
      crops_mat[,i] = c(crops_i, rep(NA, n_yrs-length(crops_i)))
    }
    kills = kills_mat
    scares = scares_mat
    crops = crops_mat

    if(is.null(act_sim)) { act_sim = sample(1:n_sims,1) } 
    acts_i = rbind(kills[,act_sim],scares[,act_sim],crops[,act_sim])
    acts_i_total = as.vector(apply(acts_i,2,sum))
    colnames(acts_i) = as.character(1:n_yrs)
    
    if(act_type == "abs") {
      act_plot = acts_i
      yl = "Total number of actions for one sample simulation"
    }
    if(act_type == "prop") { 
      act_plot = prop.table(acts_i, margin = 2)
      yl = "Proportion of actions for one sample simulation"
    }
    barplot(act_plot, 
            col = c("darkred","lightblue","darkgreen"), 
            border = c("darkred","lightblue","darkgreen"),
            space = 0,
            xlab = "Time step",
            ylab = yl)
    
  }
  
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

### Plot resources on landscape for a given single simulation run - either for a single time step,
###  or for a range of time steps (as animated GIF)

### folder = folder with output files for simulations
### sim_i = Simulation number in folder to plot. If not given, simply picks the first one.
### time_i = Time step to plot. If >0, interpreted as the time step to display. If == 0, produces an 
###  animated GIF of all time steps with given file name 
plot_resources_landscape = function(folder, sim_i = 1, time_i = 1, filename = "animation.gif") {

  opar = par()
  
  require(GMSE)
  if(time_i == 0) require(animation)
  
  source("gmse_vary_helpers.R")
  
  ### Read output files in given folder:
  out_files = list.files(folder)
  out_files = out_files[grep(".Rds", out_files)]
  out_files = paste0(folder, "/",out_files)

  ### Extract desired simulation:
  sim1 = readRDS(out_files[sim_i])
  
  ### If time_i >0:
  if(time_i>0) {
    res_pos = placeResources(sim1[[time_i]]$RESOURCES, 
                             xd = nrow(sim1[[time_i]]$LAND[,,3]), 
                             yd = nrow(sim1[[time_i]]$LAND[,,3]))
    n_on_cell = table(res_pos)
    n_cols = grey.colors(length(n_on_cell), start = 0, end = 0.5, rev = T)
    
    par(mfrow=c(1,1))
    par(mar = c(0.5,0.5,2,0.5))
    image(sim1[[time_i]]$LAND[,,3], xaxt = "n", yaxt = "n")
    image(res_pos, add = T, col = n_cols)
    mtext(sprintf("Simulation %d, time step %d", sim_i, time_i), 3, line = 0.5)
    
  }
  
  ### If time_i == 0:
  if(time_i==0) {
    
    ani.options(interval=.5)
    
    curwd = getwd()
    setwd(folder)
    saveGIF({
      for(i in 1:length(sim1)) {
        res_pos = placeResources(sim1[[i]]$RESOURCES, 
                                 xd = nrow(sim1[[i]]$LAND[,,3]), 
                                 yd = nrow(sim1[[i]]$LAND[,,3]))
        n_on_cell = table(res_pos)
        n_cols = grey.colors(length(n_on_cell), start = 0, end = 0.5, rev = T)
        
        par(mfrow=c(1,1))
        par(mar = c(0.5,0.5,2,0.5))
        image(sim1[[i]]$LAND[,,3], xaxt = "n", yaxt = "n")
        image(res_pos, add = T, col = n_cols)
        mtext(sprintf("Simulation %d, Time step %d", sim_i, i), 3, line = 0.5)
      }
    }, movie.name = filename)
    setwd(curwd)
  }
  
  par(opar)
  
}

