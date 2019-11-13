### Alias for length()
len = length

### Return number of values (len()) and number of unique values in vector:
ulen = function(x) {
  return(c(len(x), len(unique(x))))
}

### bufRange()
### 
### Takes a range of values and returns either the maximum (end="hi") or minimum (end="lo") of the range,
### plus or minus a given buffer fraction.
### If incl_val != NULL, extends either the min/max range to include a given constant.
bufRange = function(limrange, end, buffer = 0.05, incl_val=NULL) {
  limrange = limrange[!is.na(limrange)]
  
  if(end == "hi") {
    val = max(limrange)+max(limrange)*buffer
    if(!is.null(incl_val)) {
      if(incl_val>val) val = incl_val+incl_val*0.05
    }
    val = ceiling(val)
  }
  if(end == "lo") {
    val = min(limrange)-min(limrange)*buffer
    if(!is.null(incl_val)) {
      if(incl_val<val) val = incl_val-incl_val*0.05
    }
    val = floor(val)
  }
  return(val)
}

to.array = function(a) {
  I = max(unlist(lapply(a, nrow)))
  J = max(unlist(lapply(a, ncol)))
  K = len(a)
  out = array(NA, dim=c(I, J, K))
  for(k in 1:K) {
    for(j in 1:J) {
      for(i in 1:I) {
        out[i,j,k] = a[[k]][i,j]
      }
    }
  }
  return(out)
}

list.to.df = function(l) {
  return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}


