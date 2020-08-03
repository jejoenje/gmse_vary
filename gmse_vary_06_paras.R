### Make parameter grid for GMSE_VARY_06

### Varying stakeholders, ownership_var, user_yld_budget, manager_yld_budget.

# s = seq(4,32,4)
# ov = seq(0,0.95,0.2)  # ownership_var
# uyb = c(0,1)          # user_yld_budget
# av = 20               # agent_view
# to = 4                # times_observe
# J = 100               # Number of replicates per scenario
# Y = 40                # Number of years per sim
# active = 0            
# 
# par1 = expand.grid(Y = Y, J = J, done = 0, act = 0, s=s,av=av, to=to, ov = ov, uyb = uyb); rm(s, av, to, ov, uyb)
# par1$myb = par1$uyb
# par1$ub = 1000
# par1$mb = 1000
# 
# par1b = par1[par1$uyb==1,]
# par1b$ub = 100
# 
# par1 = rbind(par1, par1b)
# par0 = par1
# par0$done = NULL

# Load which ones are already one:
# load("sims/gmse_vary_06/sim_summary_file.Rdata")
#load("sims/gmse_vary_06/sim_summary.Rdata")
# 
# for(i in 1:nrow(par1)) {
#   par1$done[i] = length(
#                   which(file_data$Y == par1$Y[i] &
#                         file_data$J == par1$J[i] &
#                         file_data$s == par1$s[i] &
#                         file_data$av == par1$av[i] &
#                         file_data$to == par1$to[i] &
#                         file_data$ov == par1$ov[i] &
#                         file_data$uyb == par1$uyb[i] &
#                         file_data$myb == par1$myb[i] &
#                         file_data$ub == par1$ub[i] &
#                         file_data$mb == par1$mb[i]))
# }
#save(par1, file = "sims/gmse_vary_06/para_grid.Rdata")


# Add further (mb = 100, ub = 1000)

# load("sims/gmse_vary_06/para_grid.Rdata")
# par2 = par1[par1$ub==100,]
# par2$mb = 100
# par2$ub = 1000
# par2$done = 0
# par2$act = 0
# par1 = rbind(par1, par2)
# 
# file.copy("sims/gmse_vary_06/para_grid.Rdata", "sims/gmse_vary_06/para_grid_BACKUP.Rdata")
# save(par1, file = "sims/gmse_vary_06/para_grid.Rdata")


# Add further (mb = 100 / ub = 100 without yield-budget)

# load("sims/gmse_vary_06/para_grid.Rdata")
# 
# par1a = par1[par1$ub==100,]
# par1a$uyb = 0
# par1a$myb = 0
# par1a$done = 0
# par1b = par1[par1$mb==100,]
# par1b$uyb = 0
# par1b$myb = 0
# par1b$done = 0
# 
# par1 = rbind(par1, par1a, par1b)
# par1$batch = NULL
# 
# file.copy("sims/gmse_vary_06/para_grid.Rdata", "sims/gmse_vary_06/para_grid_BACKUP.Rdata")
# save(par1, file = "sims/gmse_vary_06/para_grid.Rdata")


### BATCH 2

s = seq(4,32,4)
ov = seq(0,0.95,0.2)  # ownership_var
uyb = 100              # user_yld_budget
myb = 100              # manager_yld_budget
ub = 1000
mb = 1000
av = 20               # agent_view
to = 4                # times_observe
J = 100               # Number of replicates per scenario
Y = 40                # Number of years per sim
active = 0

par1a = expand.grid(Y = Y, J = J, done = 0, act = 0, s=s,av=av, to=to, ov = ov, uyb = uyb, myb = myb, ub = ub, mb = mb)
par1 = rbind(par1, par1a)
rm(s, av, to, ov, uyb, myb, ub, mb, active, Y, J)

save(par1, file = "sims/gmse_vary_06/batch2_para_grid.Rdata")
