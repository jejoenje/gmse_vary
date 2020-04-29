### Produces summary figure for a set of scenarios

rm(list=ls())
source("helpers.R")
source("gmse_apply_helpers.R")
source("gmse_vary_plotting.R")

folders = c("sims/scenarioA2c", 
            "sims/scenarioA3", 
            "sims/scenarioA4a", 
            "sims/scenarioA4b", 
            "sims/scenarioA4c")

tiff("sims/scenarioA.tiff", width = 800, height = 1200, pointsize = 20)
gmse_vary_plotting(folders = folders)
dev.off()

folders = c("sims/scenario2c", 
            "sims/scenario3", 
            "sims/scenario4a", 
            "sims/scenario4b", 
            "sims/scenario4c")
 
# tiff("sims/scenario0.tiff", width = 800, height = 1200, pointsize = 20)
# gmse_vary_plotting(folders = folders)
# dev.off()

jpeg("sims/scenario0.jpeg", width = 800, height = 1200, pointsize = 20)
gmse_vary_plotting(folders = folders)
dev.off()


folders = c("sims/scenarioB2c", 
            "sims/scenarioB3", 
            "sims/scenarioB4a", 
            "sims/scenarioB4b", 
            "sims/scenarioB4c")

# tiff("sims/scenarioB.tiff", width = 800, height = 1200, pointsize = 20)
# gmse_vary_plotting(folders = folders)
# dev.off()
jpeg("sims/scenarioB.jpeg", width = 800, height = 1200, pointsize = 20)
gmse_vary_plotting(folders = folders)
dev.off()

folders = c("sims/scenarioC2c", 
            "sims/scenarioC3", 
            "sims/scenarioC4a", 
            "sims/scenarioC4b", 
            "sims/scenarioC4c")

# tiff("sims/scenarioC.tiff", width = 800, height = 1200, pointsize = 20)
# gmse_vary_plotting(folders = folders)
# dev.off()
jpeg("sims/scenarioC.jpeg", width = 800, height = 1200, pointsize = 20)
gmse_vary_plotting(folders = folders)
dev.off()




