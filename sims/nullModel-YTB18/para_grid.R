para_grid1 = expand.grid(done = 0,
                         yield_value = c(0.4),
                         tend_crop_yld = c(0.4),
                         public_land = c(0),
                         man_bud_type = c("fixed","mean","max"),
                         land_type = c("oneRich"),
                         land_type_max_frac = c(0.25,0.5,0.75),
                         ytb_type = "beta1" 
                         )

para_grid2 = expand.grid(done = 0,
                         yield_value = c(0.4),
                         tend_crop_yld = c(0.4),
                         public_land = c(0),
                         man_bud_type = c("fixed","mean","max"),
                         land_type = c("equal"),
                         land_type_max_frac = NA,
                         ytb_type = "beta1" 
)

para_grid = rbind(para_grid1, para_grid2)

write.csv(para_grid, "sims/nullModel-YTB19/para_grid.csv", row.names=F)
