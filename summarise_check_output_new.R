### Checks output sim list again full list of all expected para combs

rm(list=ls())

fullpara = read.csv("sims/para_grid_all.csv", header = T)
fullpara$done = NULL

out = as.data.frame(NULL)

#######
### YTB3 - YTB11 all simulations using buggy version of set_land()
#######
# out3 = read.csv("sims/sims_summary_YTB3.csv", header=T)
# out3$land_type_max_frac = NA
# nrow(out3[names(fullpara)]); nrow(unique(out3[names(fullpara)]))
# out4 = read.csv("sims/sims_summary_YTB4.csv", header=T)
# #out4_fullpara = out4[,(names(out4) %in% names(fullpara))]
# nrow(out4[names(fullpara)]); nrow(unique(out4[names(fullpara)]))
# out5 = read.csv("sims/sims_summary_YTB5.csv", header=T)
# #out5_fullpara = out5[,(names(out5) %in% names(fullpara))]
# nrow(out5[names(fullpara)]); nrow(unique(out5[names(fullpara)]))
# out6 = read.csv("sims/sims_summary_YTB6.csv", header=T)
# nrow(out6[names(fullpara)]); nrow(unique(out6[names(fullpara)]))
# out6 = out6[row.names(unique(out6[names(fullpara)])),]
# out7 = read.csv("sims/sims_summary_YTB7.csv", header=T)
# nrow(out7[names(fullpara)]); nrow(unique(out7[names(fullpara)]))
# out8 = read.csv("sims/sims_summary_YTB8.csv", header=T)
# nrow(out8[names(fullpara)]); nrow(unique(out8[names(fullpara)]))
# out9 = read.csv("sims/sims_summary_YTB9.csv", header=T)
# nrow(out9[names(fullpara)]); nrow(unique(out9[names(fullpara)]))
# out10 = read.csv("sims/sims_summary_YTB10.csv", header=T)
# nrow(out10[names(fullpara)]); nrow(unique(out10[names(fullpara)]))
# out = rbind(out3,out4,out5, out6, out7, out8, out9, out10)

######
### From YTB12-YTB14 onwards are sims with revised set_land()

out12 = read.csv("sims/sims_summary_YTB12.csv", header=T)
out13 = read.csv("sims/sims_summary_YTB13.csv", header=T)
out14 = read.csv("sims/sims_summary_YTB14.csv", header=T)
out = rbind(out12, out13, out14)

######
### From YTB15 onwards are sims with revised set_land() function and now 
###  setting initial (sim_old) budgets AFTER setting land ownership
###  (so starting with different values at time 0)

######
# YTB18 has tcy = 0.4 / yv = 0.4
# YTB19 has tcy 0.4, res_consume = 0.5 and yv 0.2-0.8 

#out = rbind(out, out15, out16)

### Check for duplicates:
nrow(out[,names(fullpara)])
nrow(unique(out[,names(fullpara)]))
out = out[row.names(unique(out[,names(fullpara)])),]

done = merge(fullpara, out, by = names(fullpara), all.x=TRUE, all.y=FALSE)

nrow(done)
sum(!is.na(done$idx))

write.csv(done, "sims/sims_done_new1.csv", row.names=F)
