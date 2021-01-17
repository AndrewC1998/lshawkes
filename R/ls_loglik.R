ls.loglik <- function(u, a, b, t){
  #
  #
  # Verify mu is a function
  if(is.function(u) != TRUE){
    mufunc <- function(x){
      return(u)
    }
  }else{
    mufunc <- u
  }
  # setup dummy variables
  tmax <- max(t); tmp1 <- 0; tmp2 <- 0; tmp3 <- 0
  #
  horizon <- max(t); N <- length(t); R <- c()
  #
  for(k in 1:N){
    if(k == 1){
      R[k] <- 0
      tmp1[k] <- mufunc(t[k])*(t[k])
    }else{
      R[k] <- (exp(-b*(t[k] - t[k-1])))*(1 + R[k-1])
      tmp1[k] <- mufunc(t[k])*(t[k] - t[k-1])
    }
    tmp2[k] <- log(mufunc(t[k]) + a*R[k])
    tmp3[k] <- (1 - exp(-b*(tmax - t[k])))
  }
  ll <- sum(tmp2) - sum(tmp1) - (a/b)*sum(tmp3)
  return(ll)
}
