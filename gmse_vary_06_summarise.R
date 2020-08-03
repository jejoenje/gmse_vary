rm(list=ls())
source("helpers.R")

# Mean yield as calculated from landscape (not agents array)
count_yield = function(x, type = "absolute") {
  if(type == "absolute") {
    return(tapply(x[,,2],x[,,3],sum))
  }
  if(type == "propOfMax") {
    return(
      tapply(x[,,2],x[,,3],sum)/table(x[,,3])
    )
  }
}

# Function to "pad out" vector to a given length
pad_length = function(x, len) {
  if(length(x)<len) {
    x = c(x, rep(NA, len-length(x)))
  }
  return(x)
}

pop_plot_test = function(pop, idx) {
  plot(as.vector(t(pop[1,])),type = "n", ylim = c(0,max(pop,na.rm=T)*1.1))
  apply(pop, 1, function(x) lines(as.vector(t(x))) )
}

### Set output file folder and list file names
outfiles = list.files("sims/gmse_vary_06/")
outfiles = outfiles[grepl(".Rds", outfiles)]
outfiles = paste0("sims/gmse_vary_06/",outfiles)

### Load parameter file to get number of years simmed:
load("sims/gmse_vary_06/batch3_para_grid.Rdata")

#Y =  unique(par1$Y)

## Load each file in folder, extract parameter values used, calculate no. years finished in each run.

# K = length(outfiles)
# file_data = as.data.frame(NULL)
# for(i in 1:K) {
#   print(paste(i, outfiles[i], sep = ": "))
#   # Load file
#   sim_i = readRDS(outfiles[i])
#   # Extract parameters used
#   paras_i = sim_i[[9]]
#   # Extract length of simulation run in years
#   paras_i$n_yrs =  length(unlist(lapply(sim_i$resource, nrow)))
# 
#   # Population size in years Y in sim_i:
#   res_i = unlist(lapply(sim_i$resource, nrow))
#   res_i = pad_length(res_i, paras_i$Y)
#   res_i = t(as.data.frame(res_i))
#   colnames(res_i) = paste0("res_y",as.character(rep(1:paras_i$Y)))
#   row.names(res_i) = NULL
# 
#   # Total actions:
#   # Kills:
#   kills_i = apply(list.to.df(lapply(sim_i$action, function(x) x[1,9,]))[,2:(paras_i$s+1)],1,sum)
#   kills_i = pad_length(kills_i, paras_i$Y)
#   kills_i = t(as.data.frame(kills_i))
#   colnames(kills_i) = paste0("kills_y",as.character(rep(1:paras_i$Y)))
#   # Scares:
#   scares_i = apply(list.to.df(lapply(sim_i$action, function(x) x[1,8,]))[,2:(paras_i$s+1)],1,sum)
#   scares_i = pad_length(scares_i, paras_i$Y)
#   scares_i = t(as.data.frame(scares_i))
#   colnames(scares_i) = paste0("scares_y",as.character(rep(1:paras_i$Y)))
#   # Tend-crop
#   crops_i = apply(list.to.df(lapply(sim_i$action, function(x) x[2,10,]))[,2:(paras_i$s+1)],1,sum)
#   crops_i = pad_length(crops_i, paras_i$Y)
#   crops_i = t(as.data.frame(crops_i))
#   colnames(crops_i) = paste0("crops_y",as.character(rep(1:paras_i$Y)))
# 
#   # MEAN yield across stakeholders:
#   myield_i = unlist(lapply(lapply(sim_i$land, count_yield),mean))
#   myield_i = pad_length(myield_i, paras_i$Y)
#   myield_i = t(as.data.frame(myield_i))
#   colnames(myield_i) = paste0("myield_y",as.character(rep(1:paras_i$Y))) 
#   
#   ## Variation in yield across stakeholders.
#   ## First, the proportion of total yield for each stakeholder in each time step:
#   #pyield = lapply(lapply(sim_i$land, count_yield),function(x) x/sum(x))
#   ## CV of proportional yield across all stakeholders:
#   #CVpyield_i = lapply(pyield, function(x) sd(x)/mean(x))
#   ## This is actually the same as just the CV of absolute yield across stakeholders
#   CVyield_i = lapply(lapply(sim_i$land, count_yield), function(x) sd(x)/mean(x))
#   # ... so we use the latter for the sake of parsimony!
#   CVyield_i = unlist(CVyield_i)
#   CVyield_i = pad_length(CVyield_i, paras_i$Y)
#   CVyield_i = t(as.data.frame(CVyield_i))
#   colnames(CVyield_i) = paste0("CVyield_y",as.character(rep(1:paras_i$Y))) 
#   
#   ## Mean budget across stakeholders.
#   mbudget_i = unlist(lapply(lapply(sim_i$agents, function(x) x[,26]),mean))
#   mbudget_i = pad_length(mbudget_i, paras_i$Y)
#   mbudget_i = t(as.data.frame(mbudget_i))
#   colnames(mbudget_i) = paste0("mbudget_y",as.character(rep(1:paras_i$Y))) 
#   
#   ## Variation in budget across stakeholders.
#   CVbudget_i = unlist(lapply(lapply(sim_i$agents, function(x) x[,26]),function(x) sd(x)/mean(x)))
#   CVbudget_i = pad_length(CVbudget_i, paras_i$Y)
#   CVbudget_i = t(as.data.frame(CVbudget_i))
#   colnames(CVbudget_i) = paste0("CVbudget_y",as.character(rep(1:paras_i$Y))) 
#   
#   
#   # Combine the above for in a single line:
#   paras_i = cbind(paras_i, res_i, kills_i, scares_i, crops_i, myield_i, CVyield_i, mbudget_i, CVbudget_i)
# 
#   # Add file name:
#   paras_i$file = outfiles[i]
# 
#   # Add parameters, number of years, file name as line in file_data:
#   file_data = rbind(file_data, paras_i)
# 
#   # Add '1' to correct "done" column in par1:
#   # par1_idx = which((par1$Y == paras_i$Y) &
#   #                    (par1$J == paras_i$J) &
#   #                    (par1$s == paras_i$s) &
#   #                    (par1$av == paras_i$av) &
#   #                    (par1$to == paras_i$to) &
#   #                    (par1$ov == paras_i$ov) &
#   #                    (par1$uyb == paras_i$uyb) &
#   #                    (par1$myb == paras_i$myb) &
#   #                    (par1$ub == paras_i$ub) &
#   #                    (par1$mb == paras_i$mb))
#   # par1$done[par1_idx] = par1$done[par1_idx]+1
# 
# }

### Find which parameter combinations are "unique" in file_data:
### Extract columns defining set of parameters:
# u_file_data = file_data[,(names(file_data) %in% c("Y","J","s","av","to","ov","uyb","myb","ub","mb"))]
# ### Extract unique parameter combinations:
# para_combs = unique(u_file_data)
# row.names(para_combs) = 1:nrow(para_combs)
# 
# # ### Match para_combs identifier to full list of files:
# file_data$para_comb = NA
# for(i in 1:nrow(para_combs)) {
#     match_id = which(file_data[,"Y"] == para_combs[i,"Y"] &
#       file_data[,"J"] == para_combs[i,"J"] &
#       file_data[,"s"] == para_combs[i,"s"] &
#       file_data[,"av"] == para_combs[i,"av"] &
#       file_data[,"to"] == para_combs[i,"to"] &
#       file_data[,"ov"] == para_combs[i,"ov"] &
#       file_data[,"uyb"] == para_combs[i,"uyb"] &
#       file_data[,"myb"] == para_combs[i,"myb"] &
#       file_data[,"ub"] == para_combs[i,"ub"] &
#       file_data[,"mb"] == para_combs[i,"mb"])
#     file_data$para_comb[match_id] = i
# }

#
# # ### Find which para combinations have some runs missing, and how many:
#missing = 100-table(file_data$para_comb)[which(table(file_data$para_comb) != 100)]
# if(length(missing)>0) {
#   ### Match these to para_combs table:
#   para_combs$missing = missing[match(row.names(para_combs),names(missing))]
#
#   ### Make a new "missing sims" para grid:
#   par1 = para_combs[!is.na(para_combs$missing),]
#   par1$done = par1$J-par1$missing
#   par1$act = 0
#   par1$missing = NULL
#
#   save(par1, file = "sims/gmse_vary_06/para_grid_MISSING.Rdata")
# }
#

###
### Save generated data files:
###

### All files:
# if(file.exists("sims/gmse_vary_06/batch3_summary_allfiles.Rdata")){
#   file.copy("sims/gmse_vary_06/batch3_summary_allfiles.Rdata.Rdata", "sims/gmse_vary_06/batch3_summary_allfiles.Rdata_BACKUP.Rdata")
# }
# save(file_data, file = "sims/gmse_vary_06/batch3_summary_allfiles.Rdata")

### Unique parameter combinations
# if(file.exists("sims/gmse_vary_06/batch3_summary.Rdata")){
#   file.copy("sims/gmse_vary_06/batch3_summary.Rdata", "sims/gmse_vary_06/batch3_summary_BACKUP.Rdata")
# }
# save(para_combs, file = "sims/gmse_vary_06/batch3_summary.Rdata")


load("sims/gmse_vary_06/batch3_summary_allfiles.Rdata")
load("sims/gmse_vary_06/batch3_summary.Rdata")

###
### Calculate summary statistics across runs per para comb:
###


###
### EXTINCTION SUMMARIES
### 

### Did a given sim "go extinct"?
file_data$ext = file_data$n_yrs != file_data$Y
# Rename "population" columns to something more workable:
names(file_data)[14:(14+39)] = paste0("pop_y",as.character(1:40))

### Min, max and mean number of extinctions per parameter combination:
min_n_yrs = tapply(file_data$n_yrs, file_data$para_comb, min)     # Min number of years completed across sims
max_n_yrs = tapply(file_data$n_yrs, file_data$para_comb, max)     # Max number of years completed across sims
mn_n_yrs = tapply(file_data$n_yrs, file_data$para_comb, mean)     # Mean number of years completed across sims

### Number of extinctions per parameter combination:
ext = tapply(file_data$ext, file_data$para_comb, sum)             # Number of sim runs extinct

### Match these to para_combs:
para_combs$ext  = ext[match(rownames(para_combs), names(ext))]
para_combs$min_n_yrs = min_n_yrs[match(rownames(para_combs), names(min_n_yrs))]
para_combs$max_n_yrs = max_n_yrs[match(rownames(para_combs), names(max_n_yrs))]
para_combs$mn_n_yrs = mn_n_yrs[match(rownames(para_combs), names(mn_n_yrs))]


###
### POPULATION SIZE
### 

### Mean pop size by year:
# Extract population size per year, for each sim:
pop_sizes = split(file_data[,grep("pop_", names(file_data))], file_data$para_comb)
# Mean population size per year, across simulations, by parameter combination (para_combs are rows, yrs are cols):
mn_pop_y = list.to.df(lapply(pop_sizes, function(x) colMeans(x, na.rm=T)))

# Mean percentage deviation from target:
mn_perc_dev = list.to.df(lapply(pop_sizes, function(x) colMeans((x-1000)/1000, na.rm=T)))

names(mn_pop_y) = paste0("mnPop_",1:ncol(mn_pop_y))
names(mn_perc_dev) = paste0("mnDevPop_",1:ncol(mn_perc_dev))

# Match mean pop size per year, and mean % dev per year, to para_combs:
para_combs = cbind(para_combs, mn_pop_y[match(rownames(para_combs),rownames(mn_pop_y)),])
para_combs = cbind(para_combs, mn_perc_dev[match(rownames(para_combs),rownames(mn_perc_dev)),])


###
### ACTIONS
###

# All actions, per para comb, as list:
all_kills = split(file_data[,grep("kills", names(file_data))], file_data$para_comb)
all_scares = split(file_data[,grep("scares", names(file_data))], file_data$para_comb)
all_crops = split(file_data[,grep("crops", names(file_data))], file_data$para_comb)
# Total number of actions, per para comb, as list:
total_acts = list()
p_kills = list()
p_scares = list()
p_crops = list()
for(i in 1:length(all_kills)) {
  total_acts[[i]] = all_kills[[i]]+all_scares[[i]]+all_crops[[i]]
  p_kills[[i]] =  all_kills[[i]]/total_acts[[i]]
  p_scares[[i]] =  all_scares[[i]]/total_acts[[i]]
  p_crops[[i]] =  all_crops[[i]]/total_acts[[i]]
}
# Mean PROPORTION of each action (rows = sim, cols = years)
mp_kills = round(list.to.df(lapply(p_kills, function(x) colMeans(x, na.rm=T))),3)
mp_scares = round(list.to.df(lapply(p_scares, function(x) colMeans(x, na.rm=T))),3)
mp_crops = round(list.to.df(lapply(p_crops, function(x) colMeans(x, na.rm=T))),3)

# Mean TOTAL of each action (rows = sim, cols = years)
m_kills = list.to.df(lapply(all_kills, function(x) colMeans(x, na.rm=T)))
m_scares = list.to.df(lapply(all_scares, function(x) colMeans(x, na.rm=T)))
m_crops = list.to.df(lapply(all_crops, function(x) colMeans(x, na.rm=T)))

names(m_kills) = paste0("mKills_y",as.character(1:40))
names(m_scares) = paste0("mScares_y",as.character(1:40))
names(m_crops) = paste0("mCrops_y",as.character(1:40))
names(mp_kills) = paste0("mpKills_y",as.character(1:40))
names(mp_scares) = paste0("mpScares_y",as.character(1:40))
names(mp_crops) = paste0("mpCrops_y",as.character(1:40))

# Match actions to para_combs:
para_combs = cbind(para_combs, m_kills[match(rownames(para_combs), rownames(m_kills)),])
para_combs = cbind(para_combs, m_scares[match(rownames(para_combs), rownames(m_scares)),])
para_combs = cbind(para_combs, m_crops[match(rownames(para_combs), rownames(m_crops)),])

###
### YIELD
### 

# Mean yields across stakeholders, for all sim files, split by para_comb:
mn_yields = split(file_data[,grep("myield", names(file_data))], file_data$para_comb)
mn_yields = list.to.df(lapply(mn_yields, function(x) colMeans(x, na.rm=T)))
cv_yields = split(file_data[,grep("CVyield", names(file_data))],file_data$para_comb)
mncv_yields = list.to.df(lapply(cv_yields, function(x) colMeans(x, na.rm=T)))

names(mn_yields) = paste0("mYields_",1:ncol(mn_yields))
names(mncv_yields) = paste0("mcvYields_",1:ncol(mncv_yields))

# Add to para_combs:
para_combs = cbind(para_combs, mn_yields[match(rownames(para_combs), rownames(mn_yields)),])
para_combs = cbind(para_combs, mncv_yields[match(rownames(para_combs), rownames(mncv_yields)),])



