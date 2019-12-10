para_grid1 = expand.grid(done = 0,
                         yield_value = c(0.4),
                         tend_crop_yld = c(0.2),
                         man_bud_type = c("mean"),
                         land_type = c("oneRich"),
                         land_type_max_frac = c(0.25,0.5,0.75),
                         public_land = c(0,0.25),
                         ytb_type = "beta1" 
                         )

para_grid2 = expand.grid(done = 0,
                         yield_value = c(0.4),
                         tend_crop_yld = c(0.2),
                         man_bud_type = c("mean"),
                         land_type = c("equal"),
                         land_type_max_frac = NA,
                         public_land = c(0,0.25),
                         ytb_type = "beta1"
)

para_grid = rbind(para_grid1, para_grid2)

write.csv(para_grid, "sims/nullModel-YTB20/para_grid.csv", row.names=F)
