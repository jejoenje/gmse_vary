
### set_land() - NEW VERSION

set_land = function(land, s, type, hi_frac = NULL, up_frac = NULL, lo_frac = NULL) {
  pub_land_present = FALSE
  tland = table(land)
  
  # If assuming equal land dist, return what was given:
  if(type == "equal" | is.null(type) ) {
    return(land)
  }
  
  if(type == "oneRich") {
    
    if("1" %in% names(tland)) {
      pub_land_present = TRUE
      common_land = tland[1]
      tland = tland[2:len(tland)]
      #names(tland) = as.character(1:len(tland))
    } 
    
    pland = sum(tland)
    if(is.null(up_frac)) up_frac = hi_frac+(hi_frac*0.07)
    if(is.null(lo_frac)) lo_frac = hi_frac-(hi_frac*0.07)
    
    rich_frac = rgbeta(1, mean = hi_frac, var = hi_frac/2000, min = lo_frac, max = up_frac)
    rich_size = floor(pland*rich_frac)
    
    rest = pland - rich_size
    
    others = rest/(s-1)
    others = floor(others)
    
    all = c(rich_size, rep(others, s-1))
    all[1] = all[1]+(pland-sum(all))
    
    if(pub_land_present == TRUE) {
      all = c(common_land, all)
      all_cells = as.vector(NULL)
      for(i in 1:len(all)) {
        all_cells = c(all_cells, rep(i, all[i]))
      }
    } else {
      all_cells = as.vector(NULL)
      for(i in 1:len(all)) {
        all_cells = c(all_cells, rep(i+1, all[i]))
      }  
    }
    
    land_out = matrix(all_cells, nrow = nrow(land), ncol = ncol(land))
    
    return(land_out)
  }
}