### Build parameter grid.
### 

gmse_paras = expand.grid( 
                      # "Default" GMSE parameters:
                          get_res = "Full", 
                          land_dim_1 = 200, 
                          land_dim_2 = 200,
                          land_ownership =  TRUE, 
                          res_movement = 20,               # default = 20
                          res_move_type = 0,               # was 1 (DEFAULT)
                          tend_crops =      TRUE,         # was TRUE
                          tend_crop_yld =   0.9, 
                          scaring =         TRUE,
                          minimum_cost =    10,
                          remove_pr =       0.075,       
                          res_death_type =  3,
                          lambda =          0.25,           
                          res_death_K =     10000,                   
                          RESOURCE_ini =    1000,           
                          manage_target =   1000,
                          manager_budget =  1000, 
                          user_budget =     1000,
                          public_land =     0,       
                          stakeholders =    16,              # was 8
                          res_consume =     0.2,            # was 0.2
                          observe_type =    0,
                          res_move_obs =    TRUE,
                          agent_view =      100,           
                          times_observe =   10,
                          agent_move =      100,         
                          converge_crit =   0.1,
                          ga_mingen =       40,
                      # Additional parameters to vary in gmse_apply() loop.
                          land_type = "equal",
                          land_type_max_frac = NA
            )
