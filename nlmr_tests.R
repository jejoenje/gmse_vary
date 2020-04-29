### NLMR tests
### https://ropensci.github.io/NLMR/

rm(list = ls())
library(NLMR)
library(raster)
library(rgdal)
library(landscapetools)

xd = 100
yd = 100
s = 8
K = 1000

# Simulates a patchy mosaic neutral landscape modelbased on the tessellation of a inhibition point process.
#  (nlm_mosaictess)
#
# x = nlm_mosaictess(xd,yd, germs = s)
# plot(x)
# v = table(x@data@values); v
# f = v/sum(v); round(f,3)

out1 = as.data.frame(NULL)
for(i in 1:K) {
  x = nlm_mosaictess(xd,yd, germs = s)
  v = table(x@data@values)
  f = v/sum(v)
  out1 = rbind(out1, f)
}
mean(apply(out1, 2, min))
mean(apply(out1, 2, mean))
mean(apply(out1, 2, max))

# Simulates a patchy mosaic neutral landscape modelbased on the tessellation of a random point process.
x = nlm_mosaicgibbs(xd,yd, germs = s, R = s/1000, patch_classes = s)
plot(x)
v = table(x@data@values); v
f = v/sum(v); round(f,3)

out2 = as.data.frame(NULL)
for(i in 1:K) {
  x = nlm_mosaicgibbs(xd,yd, germs = s, R = s/1000, patch_classes = s)
  v = table(x@data@values)
  f = v/sum(v)
  out2 = rbind(out2, f)
}
mean(apply(out2, 2, min))
mean(apply(out2, 2, mean))
mean(apply(out2, 2, max))
hist(out2[,1])

# Brownian motion, classified:
x = nlm_fbm(ncol = xd, nrow = yd)
plot(x)
z = util_classify(x, n = s)
plot(z)



# Mosaic field, classfied:
x = nlm_mosaicfield(ncol = xd, nrow = yd)
plot(x)
z = util_classify(x, n = s)
plot(z)

# Planar gradient, classified:
x = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
plot(x)
z = util_classify(x, n = s)
plot(z)
Z1 = z@data@values
diversity(table(Z1))
# Pick random category distribution using tesselation (for lack of better ideas)
c = nlm_mosaictess(xd,yd, germs = s)
cats = table(c@data@values)
frac = cats/sum(cats)
frac =as.vector(frac)
z = util_classify(x, weighting = frac)
plot(z)
Z2 = z@data@values

H_Z1 = diversity(table(Z1))
H_Z2 = diversity(table(Z2))
J_Z1 = diversity(table(Z1))/log(length(table(Z1)))
J_Z2 = diversity(table(Z2))/log(length(table(Z2)))
var_Z1 = var(table(Z1))
var_Z2 = var(table(Z2))

tab = cbind(c(H_Z1, H_Z2),c(J_Z1, J_Z2),c(var_Z1, var_Z2))
tab

# Attempt to reduce variance 1:
x = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
z = util_classify(x, n = s+1)
c = nlm_mosaictess(xd,yd, germs = s+1)
cats = table(c@data@values)
frac = cats/sum(cats)
frac =as.vector(frac)
z = util_classify(x, weighting = frac)
plot(z)
Z3 = z@data@values
smallest = names(head(table(Z3)[order(table(Z3))],2))
smallest = as.numeric(as.vector(smallest))
Z3[Z3==smallest[1]] = smallest[2]
Z3 = factor(Z3)
levels(Z3) = as.character(1:s)
Z3 = as.numeric(as.vector(Z3))

table(Z3)
var(table(Z3))

H_Z3 = diversity(table(Z3))
J_Z3 = diversity(table(Z3))/log(length(table(Z3)))
var_Z3 = var(table(Z3))
c(H_Z3, J_Z3, var_Z3)

tab = rbind(tab, c(H_Z3, J_Z3, var_Z3))
tab


# Attempt to reduce variance 2:
x = nlm_planargradient(ncol = xd, nrow = yd, direction = 0)
z = util_classify(x, n = s+2)
c = nlm_mosaictess(xd,yd, germs = s+2)
cats = table(c@data@values)
frac = cats/sum(cats)
frac =as.vector(frac)
z = util_classify(x, weighting = frac)
plot(z)
Z4 = z@data@values
smallest = names(head(table(Z4)[order(table(Z4))],3))
smallest = as.numeric(as.vector(smallest))
Z4[Z4==smallest[1]] = smallest[3]
Z4[Z4==smallest[2]] = smallest[3]
Z4 = factor(Z4)
levels(Z4) = as.character(1:s)
Z4 = as.numeric(as.vector(Z4))

table(Z4)
var(table(Z4))

H_Z4 = diversity(table(Z4))
J_Z4 = diversity(table(Z4))/log(length(table(Z4)))
var_Z4 = var(table(Z4))

tab = rbind(tab, c(H_Z4, J_Z4, var_Z4))
tab


# If with public land:
# Create public part first:
public_land = 0.25
public_matrix = matrix(1, ncol = xd*public_land, nrow = yd)
# Private part
x = nlm_planargradient(ncol = xd-(xd*public_land), nrow = yd, direction = 0)
plot(x)
# Pick random category distribution using tesselation (for lack of better ideas)
c = nlm_mosaictess(xd,yd, germs = s)
cats = table(c@data@values)
frac = cats/sum(cats)
frac =as.vector(frac)
z = util_classify(x, weighting = frac)
Z = z@data@values+1 # Make sure we increment identifiers by 1 ( 1 = public land)
private_matrix = matrix(Z, ncol = xd-(xd*public_land), nrow = yd)

