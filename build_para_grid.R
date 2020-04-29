### Build parameter grid.
### 

gmse_paras = expand.grid( 
                      # "Default" GMSE parameters:
                          get_res = "Full", 
                          land_dim_1 = 100, # was 200
                          land_dim_2 = 100, # was 200
                          land_ownership =  TRUE, 
                          res_movement = 20,            
                          res_move_type = 1,        
                          tend_crops =      TRUE,         
                          tend_crop_yld =   0.4,    # Was 0.1
                          scaring =         TRUE,
                          minimum_cost =    10,
                          remove_pr =       0.16,   # >0.17 seems to mean consistent declines even when manager power is high
                          res_death_type =  3,
                          lambda =          0.25,                    
                          res_death_K =     2000,          
                          RESOURCE_ini =    1000,                   
                          manage_target =   1000,         
                          manager_budget =  1000, 
                          user_budget =     1000,           
                          public_land =     0,
                          stakeholders =    12,        
                          res_consume =     0.4,        
                          observe_type =    0,
                          res_move_obs =    TRUE,
                          agent_view =      20,         # 20 seems an interesting middle ground...
                          times_observe =   5,         
                          agent_move =      100,
                          converge_crit =   0.1,
                          ga_mingen =       40,
                      # Additional parameters to vary in gmse_apply() loop.
                          # land_type = "oneRich",        # Only used in Scenario 4 (Not used by distribute_land_simplified())
                          n_years = 100,
                          res_move_to_yield = FALSE,    # Only used in Scenarios 3 and 4
                          man_bud_prop = 0.1,           # Only used in Scenarios 2-4. Was 0.125, 0.225
                          yield_type = "linear",        # Only used in Scenarios 2-4.
                          yield_value = 0.875           # Only used in Scenarios 2-4. Was 0.25, 0.5, 0.875 (1 - man_bud_prop)
                      
            )

