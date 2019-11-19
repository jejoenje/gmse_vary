### Simple test script to test looping and wrapper-ability of gmse_apply().
###
### May be deleted.
### 


### LOOP ONLY VARIANT

library(GMSE)

rm(list=ls())

# Save a list of parameter values for GMSE runs; I want this for convenience so I can store/use common paras
# safely. Note I just arbitrarily choose two here (rest kept as default).
gmse_paras = list(observe_type = 3, res_consume = 0.5, land_dim_1 = 120)

# Set number of replicates (sims) and time steps (years).
sims = 2
years = 2

# Create an empty list to store output in.
res = list()

# Loop through sims and years:

for(sim in 1:sims) {

  # For a given sim run, create an empty list with NA values, equal to each year.
  res_year = as.list(rep(NA, years))

  # Set starting point (sim_old) for a given simula$tion run.
  # Note this extracts parameter values from the list stored above.
  sim_old = gmse_apply(get_res = "Full",
                       observe_type = gmse_paras[["observe_type"]],
                       res_consume = gmse_paras[["res_consume"]],
                       land_dim_1 = gmse_paras[["land_dim_1"]]
                       )

  # Now step through the years (time steps) within year:
  for(year in 1:years) {

    # Single GMSE run using the previous sim_old list as starting point.
    # Note I rm(list=ls())want full output, hence specifying "get_res" explicitly.
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old)

    # Save this time steps' results as a list element
    res_year[[year]] = sim_new
    # Reset "sim_old" for the next time step
    sim_old <- sim_new 
    # Print some output to monitor progress
    print(sprintf("Sim %d, year %d", sim, year))
  }

  # Once all years are finished, store the sims' list (all years) as the
  # next element in the "full" output
  res[[sim]] = res_year

}

res[[1]][[2]]$get_res
res[[1]][[2]]$observe_type
res[[1]][[2]]$res_consume
res[[1]][[2]]$land_dim_1



# The above loop works fine, and seems to get the expected results.
# However, when wrapping the above into a function (so we can conveniently 
# specify and re-run simulations given numbers of years, sims and parameter 
# sets), this throughs the error 
# "Error in eval(placing_vals[[i]]) : object 'sim_paras' not found".
# Otherwise, the code is the same.
library(GMSE)

rm(list=ls())

gmse_sims = function(s, y, ...) {
  
  res = list()
  
  for(sim in 1:s) {
    
    res_year = as.list(rep(NA, y))
    
    # Note because the function is passed the parameter list as 'sim_paras', 
    # we here refer to this list instead of 'gmse_paras' in the above.

    sim_old = gmse_apply(get_res = "Full", ...)
    
    for(year in 1:y) {
      sim_new = gmse_apply(get_res = "Full", old_list = sim_old, ...)
      res_year[[year]] = sim_new
      sim_old <- sim_new
      print(sprintf("Sim %d, year %d", sim, year))
    }
    res[[sim]] = res_year
    
  }
  
  return(res)
  
}


# Save a list of parameter values for GMSE runs; I want this for convenience so I can store/use common paras
# safely. Note I just arbitrarily choose two here (rest kept as default).
gmse_paras = list(observe_type = 3, res_consume = 0.5, land_dim_1 = 120)

# Set number of replicates (sims) and time steps (years).
sims = 2
years = 2

fun_res = gmse_sims(s = 2, y = 2, 
                    observe_type = gmse_paras[["observe_type"]], 
                    res_consume = gmse_paras[["res_consume"]],
                    land_dim_1 = gmse_paras[["land_dim_1"]])

fun_res[[1]][[2]][["get_res"]]
fun_res[[1]][[2]][["observe_type"]]
fun_res[[1]][[2]][["res_consume"]]
fun_res[[1]][[2]][["land_dim_1"]]





