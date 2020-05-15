dim_1 = 100               # land_dim_1
dim_2 = 100               # land_dim_2
lo = TRUE                 # land_ownership
rbt = 2                   # res_birth_type
rdt = 2                   # res_death_type
l = 0.3                   # lambda
rpr = 0                   # remove_pr
rbK = 1e+05               # res_birth_K
rdK = 2000                # res_death_K
mt = 1000                 # manage_target
r_ini = 1000              # RESOURCE_ini
s = 8                     # stakeholders
r_cons = 0.5              # res_consume
a_view = 50               # agent_view
t_obs = 10                # times_observe
scare = TRUE              # scaring
cull = TRUE               # culling
tend_crop = TRUE          # tend_cops
tcy = 0.5                 # tend_crop_yield              # 0.475
mc = 10                   # minimum cost 

### Custom parameters for gmse_apply runs
n_years = 50                       # Number of years per sim
man_bud_prop = (1/s)               # Parameter controlling relative size of manager budget to user budgets
                                   #  NOT USED IN SCENARIO 1
yield_type = "linear"              # Parameter controlling shape of budget ~ yield function     NOT USED IN SCENARIO 1
yield_value = 0.85                 # Parameter controlling strength of budget ~ yield function  NOT USED IN SCENARIO 1

