### This script generates a large series of land distributions using distribute_land_simplified, for a range of land_var
### parameter values; for each values 1000 replicates are generated, and summaries of the minimum, maximum, mean and CV
### of land size per stakeholder are calculated. In each case, these are mean values.

land_var_series = c(0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 0.9, 0.99)
K = 100
out = as.data.frame(NULL)
s = 12
for(i in 1:length(land_var_series)) {
  out_i = as.data.frame(NULL)
  for(j in 1:K) {
    land_ij = distribute_land_simplified(xd = 100, yd = 100, s = s, land_var = land_var_series[i])
    land_ij_min = min(table(land_ij))
    land_ij_max = max(table(land_ij))
    land_ij_mean = mean(table(land_ij))
    land_ij_cv = sd(table(land_ij))/land_ij_mean
    out_i = rbind(out_i, cbind(land_ij_min, land_ij_max, land_ij_mean, land_ij_cv))
  }
  out = rbind(out, apply(out_i, 2, mean))
}
names(out) = names(out_i)

out$land_var = land_var_series
out$max_mean_ratio = out$land_ij_max/out$land_ij_mean
out = round(out,2)
out
