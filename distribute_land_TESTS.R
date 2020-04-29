rm(list=ls())

library(GMSE)
library(scales)
library(RColorBrewer)
library(doParallel)
library(truncnorm)
source('helpers.R')
source('gmse_apply_helpers.R')
source("build_para_grid.R")

gmse_paras$stakeholders = 16

sim_old = init_sims(gmse_paras)

par(mfrow=c(1,2))
table(sim_old$LAND[,,3])
table(sim_old$LAND[,,3])/sum(table(sim_old$LAND[,,3]))
plot_land(sim_old$LAND[,,3], random_order = TRUE)

# Set variable land:

lt_factor = 2
gmse_paras$land_type_max_frac = (1/gmse_paras$stakeholders)*lt_factor

new_land = distribute_land(xd = gmse_paras$land_dim_1, 
                           yd = gmse_paras$land_dim_2, 
                           s = gmse_paras$stakeholders, 
                           public_land = gmse_paras$public_land, 
                           type = gmse_paras$land_type, 
                           rich_frac = gmse_paras$land_type_max_frac)
table(new_land)
fractions = table(new_land)/sum(table(new_land))
fractions
max(fractions)/min(fractions)
plot_land(new_land)

