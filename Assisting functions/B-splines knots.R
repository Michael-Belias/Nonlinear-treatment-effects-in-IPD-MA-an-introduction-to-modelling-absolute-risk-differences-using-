###### R-function: default.knots ##########

# Computes default B-spline knots for a given
# x vector.


default.knots <- function(x,num.knots)
{
  # Delete repeated values from x
  
  x <- unique(x)
  
  # Work out the default number of knots
  
  if (missing(num.knots))
  {
    n <- length(x)
    d <- max(4,floor(n/35))
    num.knots <- floor(n/d - 1)
  }
  
  nx <- names(x)
  x <- as.vector(x)
  nax <- is.na(x)
  if(nas <- any(nax))
    x <- x[!nax]
  
  knots <- seq(0,1,length=num.knots+2)[-c(1,num.knots+2)]
  knots <- quantile(x,knots)
  
  names(knots) <- NULL
  
  return(knots)
}

########## End of default.knots ##########
