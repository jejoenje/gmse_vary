# 
# ld1 = 200
# ld2 = 200
# 
# res = RES[[1]][[100]]
# 


# Builds a matrix of positions of resources in a given RES input.
# RES is expected to be a dataframe with two columns; one for X and one for a Y coordinate of
# each individual resource (each row).
# Returns a matrix (like $LAND with NA's for no resource present, and 1 for resource present.)
# Needs the size of the landscape (land_dim1 = ld1 and land_dim_2 = ld2) too.

place_resource = function(res, ld1, ld2) {

    land = matrix(NA, nrow = ld1, ncol = ld2)
  
  for(i in 1:nrow(res)) {
    land[res[i,1],res[i,2]] = 1
  }
  
  return(land)
  
}


