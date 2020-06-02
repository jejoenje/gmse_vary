### Pause a random number of seconds to avoid clashes
Sys.sleep(runif(1, 1, 2))

### Load parameter grid
load("sims/gmse_vary_06/para_grid.Rdata")

### ID what is left to be done:
z = sum((par1$done<par1$J) & ((par1$J-par1$done) > par1$act))

while(z>0) {
  source("gmse_vary_06_SIMS.R")
  load("sims/gmse_vary_06/para_grid.Rdata")
  z = sum((par1$done<par1$J) & ((par1$J-par1$done) > par1$act))
}