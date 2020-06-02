outfiles = list.files("sims/gmse_vary_06/")
outfiles = outfiles[grepl(".Rds", outfiles)]
outfiles = paste0("sims/gmse_vary_06/",outfiles)

par_test = as.data.frame(NULL)
for(i in 1:length(outfiles)) {
  sim_i = readRDS(outfiles[i])
  paras = as.vector(sim_i[[9]][1,])
  par_test = rbind(par_test, paras)
}

par_test2 = par_test
par_test2$act = NULL
par_test2 = unique(par_test2)

head(par_test2)
nrow(par_test2)
