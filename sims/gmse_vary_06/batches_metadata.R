### Batch 1

# Varies number of stakeholders S, and ownership variation OV, for different combinations of user budget, manager
# budget, user-yield budget, manager-yield budget. Population init = 1000, target = 1000, agent_view = 20 and
# times_observe = 4 for all sims.

load("sims/gmse_vary_06/sim_summary.Rdata")

para_combs$par_set = NA

### Equal man budget / user budget, but with and without yield-budget relationship:
para_combs[para_combs$uyb == 0 & para_combs$ub == para_combs$mb,][,"par_set"] = 1
para_combs[para_combs$uyb == 1 & para_combs$ub == para_combs$mb,][,"par_set"] = 2
### No yield-budget relationship, but user budget either bigger or smaller than manager budget (by factor of 10):
para_combs[para_combs$uyb == 0 & para_combs$ub < para_combs$mb,][,"par_set"] = 3
para_combs[para_combs$uyb == 0 & para_combs$ub > para_combs$mb,][,"par_set"] = 4
### With yield-budget relationship, and user budget either bigger or smaller than manager budget (by factor of 10):
para_combs[para_combs$uyb == 1 & para_combs$ub < para_combs$mb,][,"par_set"] = 5
para_combs[para_combs$uyb == 1 & para_combs$ub > para_combs$mb,][,"par_set"] = 6

### Checks:
table(para_combs$par_set)
sum(table(para_combs$par_set))
nrow(para_combs)

### Save generated summary file:
if(file.exists("sims/gmse_vary_06/sim_summary.Rdata")){
  file.copy("sims/gmse_vary_06/sim_summary.Rdata", "sims/gmse_vary_06/sim_summary_BACKUP.Rdata")
}
save(para_combs, file = "sims/gmse_vary_06/sim_summary.Rdata")


### Batch 2

# Varies number of stakeholders S, and ownership variation OV, when budget-yield increase is >1 and increasing.

s = seq(4,32,4)
ov = seq(0,0.95,0.2)  # ownership_var
uyb = 10              # user_yld_budget
myb = 10              # manager_yld_budget
ub = 1000
mb = 1000
av = 20               # agent_view
to = 4                # times_observe
J = 100               # Number of replicates per scenario
Y = 40                # Number of years per sim
active = 0

par1 = expand.grid(Y = Y, J = J, done = 0, act = 0, s=s,av=av, to=to, ov = ov, uyb = uyb, myb = myb, ub = ub, mb = mb)
rm(s, av, to, ov, uyb, myb, ub, mb, active, Y, J)

#save(par1, file = "sims/gmse_vary_06/batch2_para_grid.Rdata")


### Batch 3

# Keeps s at 32, range 0-0.8 for ov, ub=mb=1000, but for increasing vals of uyb/myb, up from 5 -
s = 32
ov = seq(0,0.95,0.2)   # ownership_var
uyb = c(5,15,20,25,30) # user_yld_budget
myb = uyb                # manager_yld_budget
ub = 1000
mb = 1000
av = 20               # agent_view
to = 4                # times_observe
J = 100               # Number of replicates per scenario
Y = 40                # Number of years per sim
active = 0

par1 = expand.grid(Y = Y, J = J, done = 0, act = 0, s=s,av=av, to=to, ov = ov, uyb = uyb, myb = myb, ub = ub, mb = mb)
rm(s, av, to, ov, uyb, myb, ub, mb, active, Y, J)

save(par1, file = "sims/gmse_vary_06/batch3_para_grid.Rdata")

### Batch 4 - adding to batch 3 only
s = 32
ov = seq(0,0.95,0.2)    # ownership_var
uyb = c(35,40,45,50) # user_yld_budget
myb = uyb               # manager_yld_budget
ub = 1000
mb = 1000
av = 20               # agent_view
to = 4                # times_observe
J = 100               # Number of replicates per scenario
Y = 40                # Number of years per sim
active = 0

par1 = expand.grid(Y = Y, J = J, done = 0, act = 0, s=s,av=av, to=to, ov = ov, uyb = uyb, myb = myb, ub = ub, mb = mb)
rm(s, av, to, ov, uyb, myb, ub, mb, active, Y, J)

save(par1, file = "sims/gmse_vary_06/batch4_para_grid.Rdata")

