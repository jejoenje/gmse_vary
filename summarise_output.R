### Sim output summaries

rm(list=ls())
library(scales)
library(lme4)
source("helpers.R")
source("gmse_apply_helpers.R")


### nullModel-YTB2
###
### This sim set varies the YTB value (yield value) from 0.2 to 0.8, with manager_budget set as 1000 (constant).
outdir = "sims/nullModel-YTB14/out/"
outfolders = list.files(outdir)

### Collate sim output
coll_para = as.data.frame(NULL)

### Check number of files in each folder
n_files = as.vector(NULL)
for(i in 1:len(outfolders)) {
  outfolder = paste(outdir, outfolders[i], "/",sep="")
  n_files = c(n_files, len(list.files(outfolder)))
}
### Exclude output consisting of "empty" folders (paras only, i.e. failed or stopped sims)
outfolders = outfolders[n_files!=1]

for(i in 1:len(outfolders)) {
  
  outfolder = paste(outdir, outfolders[i], "/",sep="")
  outfiles = list.files(outfolder)
  
  ### Collate parameters for sim_i
  para_file = outfiles[grep("paras_", outfiles)]
  load(paste(outfolder,para_file, sep=""))
  paras_i = as.data.frame(gmse_paras)
  print(paras_i[,(names(paras_i) %in% tail(names(paras_i),7))])   ## Output
  idx_name = gsub(".Rdata", "", para_file)
  idx_name = gsub("paras_", "", idx_name)
  paras_i$idx = idx_name
  
  ### Summarise data for sim_i
  ### POP
  POP = read.csv(paste(outfolder, outfiles[grep("POP", outfiles)], sep =""),header=T)
  
  ### Crudely estimate trends using GLMM. Only do this for sims where there are more than >3 years to estimate from:
  ### Find sims with more than 3 years:
  incl_sims = as.vector(lapply(tapply(POP$N, POP$SIM, function(x) !is.na(x)),sum)>3)
  incl_sims = as.numeric(levels(factor(POP$SIM))[incl_sims])
  POP_est = POP[POP$SIM %in% incl_sims,] 
  
  POP_est$sYEAR = scale(POP_est$YEAR)
  POP_est$fSIM = factor(POP_est$SIM)
  suppressWarnings({m = glmer(N ~ sYEAR + (sYEAR|fSIM), data = POP_est, family = "poisson")})
  
  ### Conditional slopes for all sims:
  cTrends = as.vector(unlist(ranef(m)$fSIM["sYEAR"]+fixef(m)["sYEAR"]["sYEAR"]))
  cTrends = exp(cTrends)
  
  ### Rework summary data and add to sim data:
  sum_cTrends = summary(cTrends)
  sum_cTrends = sum_cTrends[c(1,3,4,6)]
  names(sum_cTrends) = paste("trend_",names(sum_cTrends),sep="")
  names_cTrends = names(sum_cTrends)
  sum_cTrends = t(data.frame(as.vector(sum_cTrends)))
  row.names(sum_cTrends) = NULL
  colnames(sum_cTrends) = names_cTrends
  paras_i = cbind(paras_i, sum_cTrends)
  
  ### EXT data:
  EXT = read.csv(paste(outfolder, outfiles[grep("EXT", outfiles)], sep =""),header=T)
  ### Total no. of extinctions, add to sim data:
  paras_i$EXT = sum(EXT$status)
  ### First extinction:
  paras_i$EXT_first = min(EXT$year, na.rm=T)
  
  
  ### Add new line to output data:
  coll_para = rbind(coll_para, paras_i)
  
}

### Reorganise columns for convenience:
col_order = c(
  which(names(coll_para)=="idx"),
  which(names(coll_para)=="sims"),
  which(names(coll_para)=="years"),
  which(names(coll_para)=="tend_crop_yield"),
  which(names(coll_para)=="land_type"),
  which(names(coll_para)=="yield_value"),
  which(names(coll_para)=="ytb_type"),
  which(names(coll_para)=="man_bud_type"),
  which(names(coll_para)=="trend_Min."),
  which(names(coll_para)=="trend_Median"),
  which(names(coll_para)=="trend_Mean"),
  which(names(coll_para)=="trend_Max."),
  which(names(coll_para)=="EXT"),
  which(names(coll_para)=="EXT_first")
)
oth_col = which(!((1:ncol(coll_para)) %in% col_order))
col_order = c(col_order, oth_col)
coll_para = coll_para[,col_order]

write.csv(coll_para, "sims/sims_summary_YTB14.csv", row.names=F)



