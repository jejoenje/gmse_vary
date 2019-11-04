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