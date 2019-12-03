### Checks output sim list again full list of all expected para combs

rm(list=ls())

fullpara = read.csv("sims/para_grid_all.csv", header = T)
fullpara$done = NULL

out4 = read.csv("sims/sims_summary_YTB4.csv", header=T)
#out4_fullpara = out4[,(names(out4) %in% names(fullpara))]

out5 = read.csv("sims/sims_summary_YTB5.csv", header=T)
#out5_fullpara = out5[,(names(out5) %in% names(fullpara))]

out = rbind(out4,out5)

done = merge(fullpara, out, by = names(fullpara), all.x=TRUE, all.y=FALSE)

nrow(done)
sum(complete.cases(done))

write.csv(done, "sims/sims_done.csv", row.names=F)
