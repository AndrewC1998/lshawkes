lambda <- function(t, data, m , alpha, beta){
  tmpl <- 0
  n <- length(data)
  for(i in 1:n){
    if(data[i] < t){
      tmpl <- tmpl + alpha*exp(-beta*(t - data[i]))
    }
  }
  lam <- m + tmpl
  return(lam)
}
