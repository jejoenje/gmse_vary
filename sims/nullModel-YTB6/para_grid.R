para_grid = expand.grid(done = 0,
                         yield_value = c(0.2,0.4,0.6,0.8),
                         tend_crop_yld = c(0.2,0.5,0.8),
                         public_land = c(0, 0.25, 0.5),
                         man_bud_type = c("max"),   ## man_bud_type = c("fixed","mean","max")
                         land_type = "oneRich",
                         land_type_max_frac = c(0.25,0.75),
                         ytb_type = "beta1" 
                         )

write.csv(para_grid, paste(gsub("/out/", "", outdir),"/para_grid8.csv", sep=""), row.names=F)
