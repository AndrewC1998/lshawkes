lshawkes.sim <- function(u, a, b, horizon){
  #
  # Verify mu is a function
  if(is.function(u) != TRUE){
    mufunc <- function(x){
      return(u)
    }
  }else{
    mufunc <- u
  }
  #
  # Setup prerequisites
  tmp <- c(); s <- 0; t <- 0
  #
  # Initialise mu
  lambda <- mufunc(0)
  #
  # further setup
  s <- s - log(runif(1))/lambda
  t <- s; tmp <- c(tmp, t)
  #
  # Enter while loop
  tmp1 <- c(lambda)
  while(s < horizon){
    U <- runif(1)
    s <- s - log(U)/lambda
    u <- runif(1)

    if(u <= (mufunc(s) + a*exp(-b*(s-t)))/lambda){
      dlambda <- a + a*exp(-b*(s-t))
      if(mufunc(s) == mufunc(tmp[length(tmp)])){
        lambda <- lambda + a
      }else{
        lambda <- mufunc(s)
      }
      t <- s
      tmp <- c(tmp, t)
    }
  }
  # Return values
  return(tmp)
}
