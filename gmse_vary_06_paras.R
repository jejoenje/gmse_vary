### Make parameter grid for GMSE_VARY_06

### Varying stakeholders, ownership_var, user_yld_budget, manager_yld_budget.

s = seq(4,32,4)
ov = seq(0,0.95,0.2)  # ownership_var
uyb = c(0,1)          # user_yld_budget
av = 20               # agent_view
to = 4                # times_observe
J = 100               # Number of replicates per scenario
Y = 40                # Number of years per sim
active = 0            

par1 = expand.grid(Y = Y, J = J, done = 0, act = 0, s=s,av=av, to=to, ov = ov, uyb = uyb); rm(s, av, to, ov, uyb)
par1$myb = par1$uyb
par1$ub = 1000
par1$mb = 1000

save(par1, file = "sims/gmse_vary_06/para_grid.Rdata")
