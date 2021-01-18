# intensity functions

intensity <- function(t, u, a, b){
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
  #
  tmp <- c()
  for(i in 1:length(t)){
    tmp[i] <- lambda(t[i], t[1:i], mufunc(t[i]), a, b)
  }
  return(tmp)
}
