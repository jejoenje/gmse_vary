rm(list=ls())
library(GMSE)

source("helpers.R")
source("gmse_vary_helpers.R")

folder = "sims/may20_scenario1_05"

out_files = list.files(folder)
out_files = out_files[grep(".Rds", out_files)]
out_files = paste0(folder, "/",out_files)

sims = list()
for(i in 1:length(out_files)) {
  sims[[i]] = readRDS(out_files[i])
}

out = sims

# Extract manage_target:
manage_targets = unique(unlist(lapply(sims, function(x) lapply(x, function(y) y$manage_target ))))
if(length(unique(manage_targets))>1) { 
  print("More than 1 manage_target values in folder!!") 
} else {
  manage_target = manage_targets[1]
}


# ID sim with highest end pop:
plot_for_act = which.max(unlist(lapply(out, length)))
layout(matrix(c(1,2,3,4,4,4), nrow = 2, byrow=T))
plot_gmse_apply(out, displaytype = "pop", pop_col = plot_for_act, manage_target = manage_target, n_yrs = 50)
plot_gmse_apply(out, displaytype = "mean_yields", n_yrs = 50)
plot_gmse_apply(out, displaytype = "mean_budgets", n_yrs = 50)
plot_gmse_apply(out, displaytype = "actions", act_type = "prop", act_sim = plot_for_act, n_yrs = 50)

#plot_resources_landscape(folder = folder, sim_i = max_i, time_i = 0, filename = "anim.gif")
