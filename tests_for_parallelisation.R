rm(list=ls())
library(GMSE)
library(scales)
library(RColorBrewer)
library(parallel)
library(foreach)
source('helpers.R')
source('gmse_apply_helpers.R')
source("build_para_grid.R")

n_sims = gmse_paras$n_sims
n_years = gmse_paras$n_years

cl = parallel::makeForkCluster(6)
doParallel::registerDoParallel(cl)

sims = list()

sims = foreach(K = 1:n_sims) %dopar% {
  sim_old = init_sims(gmse_paras)
  sims = init_sim_out(sim_old)
  
  foreach(i = 1:n_years) %do% {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
    append_output(new = sim_new, existing=sims, i = i)
  }
  
  
    
}