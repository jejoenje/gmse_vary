
load_out = function(idx) {
  
  idx = as.vector(idx)
  dfolder = paste("sims",strsplit(as.vector(idx), "_")[[1]][2],"out",idx,sep="/")
  dfiles = list.files(dfolder)
  
  dfile = dfiles[grep("EXT", dfiles)]
  ext = read.csv(paste(dfolder, dfile, sep ="/"), header=T)
  
  dfile = dfiles[grep("POP", dfiles)]
  pop = read.csv(paste(dfolder, dfile, sep ="/"), header=T)
  
  # sum(ext$status)
  # 
  # out = as.vector(NULL)
  # for(i in 1:max(pop$SIM)) {
  #   pop_i = pop[pop$SIM==i, ]
  #   out = c(out, (sum(is.na(pop_i$N) | pop_i$N == 0)) )
  # }
  # sum(out!=0)
  
  return(list(pop = pop, ext = ext))
  
}