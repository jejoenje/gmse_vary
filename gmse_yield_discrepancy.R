### Checking GMSE yield counts - 

rm(list = ls())
library(GMSE)

# Example w/o tending crops:
test = gmse(time_max = 10, land_ownership = TRUE, stakeholders = 4, scaring = TRUE, tend_crops = FALSE)

# For time step k...
k = 1
# User yields listed in AGENTS:
test$agents[[k]][2:5,16]
# This is the same as
tapply(test$land[[k]][,,2], test$land[[k]][,,3], sum)
# So the difference is 0:
test$agents[[k]][2:5,16] - tapply(test$land[[k]][,,2], test$land[[k]][,,3], sum)

# Same as for random other time steps:
k = 3
test$agents[[k]][2:5,16] - tapply(test$land[[k]][,,2], test$land[[k]][,,3], sum)
k = 9
test$agents[[k]][2:5,16] - tapply(test$land[[k]][,,2], test$land[[k]][,,3], sum)

# So all good so far.


### Now trying this with tend_crops = TRUE:
test2 = gmse(time_max = 10, land_ownership = TRUE, stakeholders = 4, scaring = TRUE, tend_crops = TRUE)
k = 1
test2$agents[[k]][2:5,16] - tapply(test2$land[[k]][,,2], test2$land[[k]][,,3], sum)
k = 2
test2$agents[[k]][2:5,16] - tapply(test2$land[[k]][,,2], test2$land[[k]][,,3], sum)
k = 9
test2$agents[[k]][2:5,16] - tapply(test2$land[[k]][,,2], test2$land[[k]][,,3], sum)


### If the difference between the yield values in AGENTS and LAND is due to resource damage alone, this should 
###  mean it should disappear if the resource does not consume:
test3 = gmse(time_max = 10, land_ownership = TRUE, stakeholders = 4, scaring = FALSE,
             tend_crops = TRUE, tend_crop_yld = 1, res_consume = 0.85, RESOURCE_ini = 2000, res_death_K = 2000)
k = 1
test3$agents[[k]][2:5,16] - tapply(test3$land[[k]][,,2], test3$land[[k]][,,3], sum)
k = 2
test3$agents[[k]][2:5,16] - tapply(test3$land[[k]][,,2], test3$land[[k]][,,3], sum)
k = 9
test3$agents[[k]][2:5,16] - tapply(test3$land[[k]][,,2], test3$land[[k]][,,3], sum)

temp = matrix(NA, nrow = 10, ncol = 4)
for(i in 1:10) {
  temp[i,] = as.vector(tapply(test3$land[[i]][,,2],test3$land[[i]][,,3],sum)/table(test3$land[[i]][,,3]))
}
temp
