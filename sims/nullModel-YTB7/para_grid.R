para_grid = expand.grid(done = 0,
                         yield_value = c(0.2,0.4,0.6,0.8),
                         tend_crop_yld = c(0.2),
                         public_land = c(0, 0.25, 0.5),
                         man_bud_type = c("fixed","mean","max"),
                         land_type = "equal",
                         land_type_max_frac = NA,
                         ytb_type = "beta1" 
                         )

write.csv(para_grid, "sims/nullModel-YTB7/para_grid.csv", row.names=F)
