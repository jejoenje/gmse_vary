### Testing distributions for stakeholder budgets
### 

sample_budgets1 = function(N = 1000, shape = 1, scale = 1, max_scale = 2, manager_budget = 1000) {
  
  out = rgamma(N, shape = shape, scale = scale)*1000
  
  out[out>manager_budget*max_scale] = manager_budget*max_scale
  
  # Very poor:
  poor = which(out<(manager_budget/10))
  out[poor] = out[poor] + sample((manager_budget*0.25):(manager_budget*0.5), size = length(out[poor]), replace=T)
  
  return(out)
  
}

shp = 0.5
scl = 2
max_scl = 2
man_bud = 1000

out = sample_budgets(N = 1000, shape = shp, scale = 1, max_scale = max_scl, manager_budget = man_bud)
par(mfrow=c(1,2))
hist(out)
test_out = floor(sample(out, 8))
test_out
hist(test_out, breaks = 1000, xlim=c(0,man_bud*max_scl))



rm(list=ls())
shp1 = 5
shp2 = 2
man_bud = 1000
sample_budgets2 = function(N = NULL, manager_budget = NULL, shape1 = NULL, shape2 = NULL) {
  return(rbeta(N, shape1, shape2)*manager_budget)
}

out = sample_budgets2(N = 1000, shape1 = shp1, shape2 = shp2, manager_budget = man_bud)
hist(out)
test_out = floor(sample(out, 8))
abline(v = test_out, col="red")
abline(v = mean(out), col="blue", lwd =2)



sample_budgets_ManyPoorFewRich = function(nstakeholders = 8, manager_budget = 1000, scale = 1) {
  return(floor(rbeta(nstakeholders, 1, .5)*(manager_budget*scale)))
}
hist(sample_budgets_ManyPoorFewRich(nstakeholders = 1000, scale = 1))
hist(sample_budgets_ManyPoorFewRich(nstakeholders = 1000, scale = 1.5))


