rm(list=ls())
source("helpers.R")
source("gmse_apply_helpers.R")
library(NLMR)

typelist = c("fbm","fbm","fbm","fbm","mosaictess","mosaicgibbs","mosaicgibbs","mosaicgibbs","mosaicgibbs",
             "gaussianfield","gaussianfield","gaussianfield")
r = c(rep(NA,5),1,10,100,1000,rep(NA,3))
fbm_fract_dim = c(0.001,0.1,1,1.9,rep(NA,8))
nug = c(rep(NA,9),0.001,0.01,0.1)
out = as.data.frame(NULL)
K = 100
out = as.data.frame(NULL)
for(i in 1:length(typelist)) {
  out_i = as.data.frame(NULL)
  print(typelist[i])
  for(k in 1:K) {
    temp = distribute_land_nlm(xd = 100, yd = 100, s = 8, type = typelist[i], fbm_fract_dim = fbm_fract_dim[i], 
                               r = r[i], nug = nug[i])
    out_i = rbind(out_i, summarise_land(temp, type="prop"))
  }
  out = rbind(out, cbind(typelist[i], fbm_fract_dim[i], r[i], mean(out_i$cv), sd(out_i$cv)))
}
out[,"V4"] = round(as.numeric(as.vector(out[,"V4"])),3)
out[,"V5"] = round(as.numeric(as.vector(out[,"V5"])),3)
out = round(cbind(out, out[,"V5"]/out[,"V4"]),3)

temp = as.vector(NULL)
for(i in 1:1000) {
  land = distribute_land_nlm(xd = 100, yd = 100, s = 8, type = "fbm", fbm_fract_dim = 0.001)
  temp = c(temp, as.vector(table(land)))
}


pick_breaks = function(K = 8, s1 = 1, sd = 2) {
  temp = rbeta(K, s1, s2)
  temp = temp/sum(temp)
}

beta_land = function(xd = 100, yd = 100, s = 8, s1 = 1, s2 = 1, r = NULL) {
  if(!is.null(r)) {
    temp = rbeta(s, s1, s1*r)
  } else {
    temp = rbeta(s, s1, s2)
  }
  temp = temp/sum(temp)
  gradient = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
  land = util_classify(gradient, weighting = temp)
  v = land@data@values
  land = matrix(v, xd, yd)
  return(land)
}

land = beta_land(s1 = 1, s2 = 1)
table(land)/sum(land)
plot_land(land)

land = beta_land(s1 = 1, s2 = 2)
table(land)/sum(land)
plot_land(land)

out = as.data.frame(NULL)
for(i in 1:1000) {
  land = beta_land(s1 = 1.75, r = 2)
  out = rbind(out, c(min(table(land)),max(table(land)),sd(table(land))))
}
names(out) = c("min","max","sd")
apply(out, 2, mean)

par(mfrow=c(1,1))
plot_land(beta_land(s1 = 1, r = 1.3))



beta_land2 = function(xd = 100, yd = 100, s = 8, randomise = FALSE, s1 = 1, s2 = 1, r = NULL, f = 1, fvar = 0) {
  require(truncnorm)
  
  if(fvar>0) {
    hi_frac = rtruncnorm(1, mean = (1/s)*f, sd = ((1/s)*f)/fvar, a = 0, b = 1)
  } else {
    hi_frac = (1/s)*f
  }
  rest = 1-hi_frac
  
  if(randomise == FALSE) {
    temp = c(rep(rest/(s-1),s-1),hi_frac)
  } 
  if(randomise == TRUE) {
    
    if(!is.null(r)) {
      temp = rbeta(s-1, s1, s1*r)
    } else {
      temp = rbeta(s-1, s1, s2)
    }  
    temp = temp/sum(temp)
    temp = c(temp*rest,hi_frac)
  }
  
  gradient = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
  land = util_classify(gradient, weighting = temp)
  v = land@data@values
  land = matrix(v, xd, yd)
  return(land)
  
}

test = beta_land2(s = 16, randomise = TRUE, s1 = 1, s2 = 1.05, f = 4, fvar = 0)
plot_land(test)



rm(list = ls())
library(NLMR)
library(raster)
library(rgdal)
library(landscapetools)
source("helpers.R")
source("gmse_apply_helpers.R")

xd = 100
yd = 100
s = 8
K = 1000
f = 2

summarise_land = function(land, type = "absolute") {
  require(vegan)
  
  if(type == "absolute") {
    H = diversity(table(land))
    J = H/log(length(table(land)))
    v = var(table(land))
    cv = var(table(land))/mean(table(land))
  }
  
  if(type == "prop") {
    pland = table(land)/sum(table(land))
    H = diversity(pland)
    J = H/log(length(pland))
    v = var(pland)
    cv = var(pland)/mean(pland)
  }
  
  
  return(data.frame(H = H, J = J, var = v, cv = cv))
}

create_land_raster = function(xd, yd, s, f = 0) {
  
  base_gradient = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
  
  S = s+f
  
  c = nlm_mosaictess(xd,yd, germs = S)
  cats = table(c@data@values)
  frac = cats/sum(cats)
  frac = as.vector(frac)
  unequal_gradient = util_classify(base_gradient, weighting = frac)
  
  v = unequal_gradient@data@values
  
  if(S>s) {
    combine_cats = names(table(v)[order(table(v))][1:((S-s)+1)])
    merge_from = as.numeric(head(combine_cats, S-s))
    merge_to = as.numeric(tail(combine_cats, 1))
    
    v[v %in% merge_from] = merge_to
    
    # Relevel
    vF = factor(v)
    levels(vF) = as.character(1:s)
    v = as.numeric(as.vector(vF))
    v = v+1
    v = v[order(v)]
    
    land = matrix(v, nrow = yd, ncol = xd)
    
  } else {
    v = unequal_gradient@data@values+1
    land = matrix(v, nrow = yd, ncol = xd)
  }
  
  return(land)
}

test = create_land_raster(xd = xd, yd = yd, s = s, f = 0)
plot_land(test)
summarise_land(test)

out0 = as.data.frame(NULL)
for(i in 1:100) {
  out0 = rbind(out0, summarise_land(create_land_raster(xd = xd, yd = yd, s = 16, f = 0)) )  
}
out1 = as.data.frame(NULL)
for(i in 1:100) {
  out1 = rbind(out1, summarise_land(create_land_raster(xd = xd, yd = yd, s = 16, f = 1)) )  
}
out2 = as.data.frame(NULL)
for(i in 1:100) {
  out2 = rbind(out2, summarise_land(create_land_raster(xd = xd, yd = yd, s = 16, f = 2)) )  
}
par(mfrow=c(3,3))
hist(out0[,1], xlim = c(1.6, 2.15))
hist(out0[,2], xlim = c(0.75, 1))
hist(out0[,3], xlim = c(0, max(c(out0[,3],out1[,3],out2[,3]))))

hist(out1[,1], xlim = c(1.6, 2.15))
hist(out1[,2], xlim = c(0.75, 1))
hist(out1[,3], xlim = c(0, max(c(out0[,3],out1[,3],out2[,3]))))

hist(out2[,1], xlim = c(1.6, 2.15))
hist(out2[,2], xlim = c(0.75, 1))
hist(out2[,3], xlim = c(0,max(c(out0[,3],out1[,3],out2[,3]))))



# Brownian motion, classified:
x = nlm_fbm(ncol = xd, nrow = yd)
plot(x)
z = util_classify(x, n = s)
#plot(z)
v = z@data@values
v = v[order(v)]
V = matrix(v, xd, yd)
plot_land(V)
table(V)

nlm_dist = function(xd = 100, yd = 100, s = 16, type = "fbm", mpd_rand_dev = 1, fbm_fract_dim = 0.8) {
  allowed_types = c("fbm", "mosaictess", "mosaicgibbs","planargradient","mpd","uniform")
  if( !(type %in% allowed_types) ) stop("Given type not yet implemented.")
  
  if(type == "uniform") {
    v = sample(1:s, size = xd*yd, replace = TRUE)
    v = v[order(v)]
    land = matrix(v, xd, yd)
    return(land)
  }
  
  if(type == "planargradient") {
    x = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
    x = util_classify(x, n = s)
    v = x@data@values
    v = v+1
    land = matrix(v, xd, yd)
    return(land)
  }
  
  if(type == "fbm") {
    x = nlm_fbm(ncol = xd, nrow = yd, fract_dim = fbm_fract_dim)
    z = util_classify(x, n = s)
    v = z@data@values
    v = v[order(v)]
    V = matrix(v, xd, yd)
    return(V)  
  }
  if(type == "mosaictess") {
    x = nlm_mosaictess(xd, yd, germs = s, rescale = F)
    vF = factor(x@data@values)
    levels(vF) = as.character(1:s)
    v = as.numeric(as.vector(vF))
    v = v[order(v)]
    v = v+1
    land = matrix(v, xd, yd)
    return(land)
  }
  if(type == "mosaicgibbs") {
    x = nlm_mosaicgibbs(xd, yd, germs = s, R = s/10, patch_classes = s)
    vF = factor(x@data@values)
    levels(vF) = as.character(1:s)
    v = as.numeric(as.vector(vF))
    v = v[order(v)]
    v = v+1
    land = matrix(v, xd, yd)
    return(land)
  }
  if(type == "mpd") {
    x = nlm_mpd(ncol = xd, nrow = yd, rand_dev = mpd_rand_dev)
    x = util_classify(x, n = s)
    #vF = factor(x@data@values)
    #levels(vF) = as.character(1:s)
    #v = as.numeric(as.vector(vF))
    #v = v[order(v)]
    v = v+1
    land = matrix(v, xd, yd)
    return(land)
  }
  
}



