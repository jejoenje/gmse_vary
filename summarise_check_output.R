### Checks output sim list again full list of all expected para combs

rm(list=ls())

fullpara = read.csv("sims/para_grid_all.csv", header = T)
fullpara$done = NULL

out = read.csv("sims/sims_summary_YTB4.csv", header=T)
out_fullpara = out[,(names(out) %in% names(fullpara))]

done = merge(fullpara, out, by = names(fullpara), all.x=TRUE, all.y=FALSE)
