### Sim output summaries

rm(list=ls())
library(scales)
library(lme4)
source("helpers.R")

### nullModel-YTB2
###
### This sim set varies the YTB value (yield value) from 0.2 to 0.8, with manager_budget set as 1000 (constant).
outdir = "sims/nullModel-YTB3/out/"
outfolders = list.files(outdir)

### Collate sim output
coll_para = as.data.frame(NULL)

for(i in 1:len(outfolders)) {
  
  outfolder = paste(outdir, outfolders[i], "/",sep="")
  outfiles = list.files(outfolder)
  
  ### Collate parameters for sim_i
  para_file = outfiles[grep("paras_", outfiles)]
  load(paste(outfolder,para_file, sep=""))
  paras_i = as.data.frame(gmse_paras)
  idx_name = gsub(".Rdata", "", para_file)
  idx_name = gsub("paras_", "", idx_name)
  paras_i$idx = idx_name
  
  ### Summarise data for sim_i
  ### POP
  POP = read.csv(paste(outfolder, outfiles[grep("POP", outfiles)], sep =""),header=T)
  
  ### Fit glmer model to POP data to crudely average trend:
  POP$sYEAR = scale(POP$YEAR)
  POP$fSIM = factor(POP$SIM)
  suppressWarnings({m = glmer(N ~ sYEAR + (sYEAR|fSIM), data = POP, family = "poisson")})
  ### PLOT ESTIMATED TRENDS/SLOPES:
  # plot(POP$YEAR, POP$N, type = "n")
  # 
  # 
  # sims = as.numeric(levels(factor(POP$SIM)))
  # for(i in 1:len(sims)) {
  #   lines(POP$YEAR[POP$SIM==i], POP$N[POP$SIM==i], col = alpha("grey",0.75))
  # }
  # 
  # for(i in 1:nlevels(POP$fSIM)) {
  #   pvals = data.frame(sYEAR = yearrange, fSIM = factor(i))
  #   p1 = predict(m, newdata = pvals, type = "response")
  #   lines(POP$YEAR[POP$SIM==i], p1, col = "red")
  # }
  
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
  paras_i$EXT_first = min(EXT$year)
  
  
  ### Add new line to output data:
  coll_para = rbind(coll_para, paras_i)
  
}

### Reorganise columns for convenience:
col_order = c(
  which(names(coll_para)=="idx"),
  which(names(coll_para)=="sims"),
  which(names(coll_para)=="years"),
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





