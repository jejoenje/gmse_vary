### Alias for length()
len = length

### Generalised beta distribution with mean, var, min and max.
rgbeta <- function(n, mean, var, min = 0, max = 1)
{
  dmin <- mean - min
  dmax <- max - mean
  
  if (dmin <= 0 || dmax <= 0)
  {
    stop(paste("mean must be between min =", min, "and max =", max)) 
  }
  
  if (var >= dmin * dmax)
  {
    stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
  }
  
  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- var / (max - min)^2
  
  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)
  
  # generate standard beta observations and transform
  x <- rbeta(n, a, b)
  y <- (max - min) * x + min
  
  return(y)
}

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


