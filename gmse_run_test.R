test = gmse(time_max = 2,
            land_dim_1 = 100,
            land_dim_2 = 100,
            land_ownership = T,
            res_birth_type = 2,
            res_death_type = 2,
            lambda = 3,
            remove_pr = 0,
            res_birth_K = 2000,
            res_death_K = 2000,
            manage_target = 1000,
            RESOURCE_ini = 1000,
            stakeholders = 8,
            res_consume = 0.5,
            agent_view = 50,
            times_observe = 10,
            scaring = TRUE,
            culling = TRUE,
            tend_crops = TRUE,
            tend_crop_yld = 0.5,
            minimum_cost = 10)
plot_gmse_results(test)
nrow(test$resource[[2]])

rm(list=ls())
pop = as.vector(NULL)
sim_old = gmse_apply(get_res = "Full",
           land_dim_1 = 100,
           land_dim_2 = 100,
           land_ownership = T,
           res_birth_type = 2,
           res_death_type = 2,
           lambda = 3,
           remove_pr = 0,
           res_birth_K = 100000,
           res_death_K = 2000,
           manage_target = 1000,
           RESOURCE_ini = 1000,
           stakeholders = 8, 
           res_consume = 0.5,
           agent_view = 50,
           times_observe = 10,
           scaring = TRUE, 
           culling = TRUE,
           tend_crops = TRUE, 
           tend_crop_yld = 0.5,
           minimum_cost = 10)
pop = c(pop, sim_old$basic_output$resource_results)
pop[1]

# for(i in 2:50) {
#   sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
#   pop = c(pop, sim_new$basic_output$resource_results)
#   print(pop[i])
#   sim_old = sim_new
# }

rm(list=ls())
source("parasMay2020.R")
pop = as.vector(NULL)

sim_old = gmse_apply(get_res = "Full",
                     land_dim_1 = dim_1,
                     land_dim_2 = dim_2,
                     land_ownership = lo,
                     res_birth_type = rbt,
                     res_death_type = rdt,
                     lambda = l,
                     remove_pr = rpr,
                     res_birth_K = rbK,
                     res_death_K = rdK,
                     manage_target = mt,
                     RESOURCE_ini = r_ini,
                     stakeholders = s, 
                     res_consume = r_cons,
                     agent_view = a_view,
                     times_observe = t_obs,
                     scaring = scare, 
                     culling = cull,
                     tend_crops = tend_crop, 
                     tend_crop_yld = tcy,
                     minimum_cost = mc)
pop = c(pop, sim_old$basic_output$resource_results)
pop[1]


rm(list=ls())
source("gmse_run_test_PARAS.R")
pop = as.vector(NULL)

sim_old = gmse_apply(get_res = "Full",
                     land_dim_1 = LAND_DIM_1,
                     land_dim_2 = LAND_DIM_2,
                     land_ownership = LAND_OWNERSHIP,
                     res_birth_type = RES_BIRTH_TYPE,
                     res_death_type = RES_DEATH_TYPE,
                     lambda = LAMBDA,
                     remove_pr = REMOVE_PR,
                     res_birth_K = RES_BIRTH_K,
                     res_death_K = RES_DEATH_K,
                     manage_target = MANAGE_TARGET,
                     RESOURCE_ini = RESOURCE_INI,
                     stakeholders = STAKEHOLDERS, 
                     res_consume = RES_CONSUME,
                     agent_view = AGENT_VIEW,
                     times_observe = TIMES_OBSERVE,
                     scaring = SCARING, 
                     culling = CULLING,
                     tend_crops = TEND_CROP, 
                     tend_crop_yld = TEND_CROP_YLD,
                     minimum_cost = MINIMUM_COST)
pop = c(pop, sim_old$basic_output$resource_results)
pop[1]




### Test with default:

rm(list=ls())
test_gmse = gmse(time_max = 10, plotting = F)
pop_gmse = unlist(lapply(test_gmse$resource, nrow))

sim_old = gmse_apply(get_res = "Full")
pop_gmseapply = sim_old$basic_output$resource_results
for(i in 2:10) {
  sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
  pop_gmseapply = c(pop_gmseapply, sim_new$basic_output$resource_results)
  sim_old = sim_new
}
cbind(pop_gmse, pop_gmseapply)


### Test with some parameters tweaked:
rm(list=ls())
test_gmse2 = gmse(time_max = 10, agent_view = 20, times_observe = 2, plotting = F)
pop_gmse2 = unlist(lapply(test_gmse2$resource, nrow))

sim_old2 = gmse_apply(get_res = "Full", agent_view = 20, times_observe = 2)
pop_gmseapply2 = sim_old2$basic_output$resource_results
for(i in 2:10) {
  sim_new2 = gmse_apply(get_res = "Full", old_list = sim_old2)
  pop_gmseapply2 = c(pop_gmseapply2, sim_new2$basic_output$resource_results)
  sim_old2 = sim_new2
}
cbind(pop_gmse2, pop_gmseapply2)









### Tests with parameter names:
rm(list=ls())
LAND_OWNERSHIP = TRUE
AGENT_VIEW = 20
TIMES_OBSERVE = 2
STAKEHOLDERS = 8
test_gmse3 = gmse(time_max = 10, agent_view = AGENT_VIEW, times_observe = TIMES_OBSERVE, plotting = F)
pop_gmse3 = unlist(lapply(test_gmse3$resource, nrow))

sim_old3 = gmse_apply(get_res = "Full", agent_view = AGENT_VIEW, times_observe = TIMES_OBSERVE)
pop_gmseapply3 = sim_old3$basic_output$resource_results
for(i in 2:10) {
  sim_new3 = gmse_apply(get_res = "Full", old_list = sim_old3)
  pop_gmseapply3 = c(pop_gmseapply3, sim_new3$basic_output$resource_results)
  sim_old3 = sim_new3
}
cbind(pop_gmse3, pop_gmseapply3)

