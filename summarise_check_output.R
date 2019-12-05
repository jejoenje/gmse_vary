### Checks output sim list again full list of all expected para combs

rm(list=ls())

fullpara = read.csv("sims/para_grid_all.csv", header = T)
fullpara$done = NULL

out3 = read.csv("sims/sims_summary_YTB3.csv", header=T)
out3$land_type_max_frac = NA

out4 = read.csv("sims/sims_summary_YTB4.csv", header=T)
#out4_fullpara = out4[,(names(out4) %in% names(fullpara))]

out5 = read.csv("sims/sims_summary_YTB5.csv", header=T)
#out5_fullpara = out5[,(names(out5) %in% names(fullpara))]

out6 = read.csv("sims/sims_summary_YTB6.csv", header=T)
out7 = read.csv("sims/sims_summary_YTB7.csv", header=T)
out8 = read.csv("sims/sims_summary_YTB8.csv", header=T)

out = rbind(out3,out4,out5, out6, out7, out8)

done = merge(fullpara, out, by = names(fullpara), all.x=TRUE, all.y=FALSE)

nrow(done)
sum(complete.cases(done))

write.csv(done, "sims/sims_done.csv", row.names=F)
