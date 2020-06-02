### Using edits in branch `jeroen` on 20/05/2020:
# 
# test = gmse(time_max = 50, land_ownership = TRUE, usr_yld_budget = 0, man_yld_budget = 0,
#             plotting = T, stakeholders = 4, tend_crops = TRUE,
#             tend_crop_yld = 0.5, ownership_var = 0, res_consume = 0.5,
#             lambda = 0.3, manager_budget = 1000, user_budget = 1000,
#             scaring = TRUE, RESOURCE_ini = 1000, manage_target = 1000,
#             res_death_K = 2000, agent_view = 10, times_observe = 1, perceive_scare = NA)

### Inreasing the number of stakeholders seems to increase prob of extinction, all else being equal.

### I suspect that by increasing variability among stakeholders (e.g. by yield_budget >0 and ownership_var > 0 ) will 
###  decrease probability of extinction, as the number of stakeholders increases.

rm(list=ls())

s = seq(4,32,4)
ov = c(0,0.5,0.95)  # ownership_var
uyb = c(0,1)        # user_yld_budget
av = 20             # agent_view
to = 4              # times_observe
J = 10              # Number of replicates per scenario
Y = 40              # Number of years per sim
vpars = expand.grid(s=s,av=av, to=to, ov = ov, uyb = uyb); rm(s, av, to, ov, uyb)
vpars$myb = vpars$uyb   # manager_yield_budget
vpars$ub = 10
vpars$ub[vpars$uyb == 0] = 1000
vpars$mb = vpars$ub

gmse_vary_06 = list()
for(i in 1:nrow(vpars)) {
    gmse_vary_06_i = list()
    for(j in 1:J) {
        print(sprintf("Scenario %d, rep %d", i, j))
        gmse_vary_06_i[[j]] = gmse(time_max = Y, 
                           RESOURCE_ini = 1000, 
                           lambda = 0.3, 
                           res_death_K = 2000,
                           res_consume = 0.5, 
                           manage_target = 1000, 
                           land_ownership = TRUE,
                           scaring = TRUE,
                           tend_crops = TRUE,
                           tend_crop_yld = 0.5,
                           usr_yld_budget = vpars[i,"uyb"], 
                           man_yld_budget = vpars[i,"myb"], 
                           stakeholders = vpars[i,"s"],
                           ownership_var = vpars[i,"ov"],
                           manager_budget = vpars[i,"mb"], 
                           user_budget = vpars[i,"ub"], 
                           agent_view = vpars[i,"av"], 
                           times_observe = vpars[i,"to"],
                           plotting = T
                           )    
    }
    gmse_vary_06[[i]] = gmse_vary_06_i
}


### Summarise output

### Count number of years and number of extinctions:
nyrs = matrix(NA, nrow = nrow(vpars), ncol = J)
for(i in 1:nrow(vpars)) {
    nyrs[i,] = unlist(lapply(gmse_vary_06[[i]], function(x) length(x$resource)))
}
vpars$yrs_mn = apply(nyrs,1,mean)
vpars$n_ext = apply((nyrs!=Y), 1, sum)/J


### Some more:

vpars_new
gmse_vary_06_new = list()
for(i in 1:nrow(vpars_new)) {
    gmse_vary_06_new_i = list()
    for(j in 1:J) {
        print(sprintf("Scenario %d, rep %d", i, j))
        gmse_vary_06_new_i[[j]] = gmse(time_max = Y, 
                                   RESOURCE_ini = 1000, 
                                   lambda = 0.3, 
                                   res_death_K = 2000,
                                   res_consume = 0.5, 
                                   manage_target = 1000, 
                                   land_ownership = TRUE,
                                   scaring = TRUE,
                                   tend_crops = TRUE,
                                   tend_crop_yld = 0.5,
                                   usr_yld_budget = vpars_new[i,"uyb"], 
                                   man_yld_budget = vpars_new[i,"myb"], 
                                   stakeholders = vpars_new[i,"s"],
                                   ownership_var = vpars_new[i,"ov"],
                                   manager_budget = vpars_new[i,"mb"], 
                                   user_budget = vpars_new[i,"ub"], 
                                   agent_view = vpars_new[i,"av"], 
                                   times_observe = vpars_new[i,"to"],
                                   plotting = T
        )    
    }
    gmse_vary_06_new[[i]] = gmse_vary_06_new_i
}



### Summarise output and add to vpars:
OV0_nyrs = matrix(NA, nrow = nrow(vpars), ncol = J)
OV25_nyrs = matrix(NA, nrow = nrow(vpars), ncol = J)
OV05_nyrs = matrix(NA, nrow = nrow(vpars), ncol = J)

for(i in 1:nrow(vpars)) {
    for(j in 1:J) {
        OV0_nyrs[i,j] = length(lapply(out1[[i]][[j]]$resource,nrow))
        OV25_nyrs[i,j] = length(lapply(out2[[i]][[j]]$resource,nrow))
        OV05_nyrs[i,j] = length(lapply(out3[[i]][[j]]$resource,nrow))
    }
}

vpars$OV0_mnnyrs = apply(OV0_nyrs, 1, mean)
vpars$OV0_pext = apply((OV0_nyrs != 40), 1, sum)/J
vpars$OV25_mnnyrs = apply(OV25_nyrs, 1, mean)
vpars$OV25_pext = apply((OV25_nyrs != 40), 1, sum)/J
vpars$OV05_mnnyrs = apply(OV05_nyrs, 1, mean)
vpars$OV05_pext = apply((OV05_nyrs != 40), 1, sum)/J

