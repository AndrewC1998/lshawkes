plot.gen <- function(u, a, b, t){
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
  #
  #
  tmp <- intensity(t,u,a,b)
  tmp2 <- cpt.np(tmp, penalty = "Manual", pen.value = max(t))@cpts
  plot(t, tmp, type = "l",
       ylab = TeX('$\\lambda(t)$'), xlab = "t", lwd = 2)
  grid()
  abline(v = t[tmp2], lwd = 2, col = "blue")

  for(i in 1:length(tmp2)){
    if(i == 1){
      tmp3 <- (tmp2[i]/t[tmp2[i]])*(1 - a/b)
      segments(0, tmp3, t[tmp2[i]], tmp3, lwd = 2, col = "red")
    }else{
      tmp3 <- ((tmp2[i] - tmp2[i-1])/(t[tmp2[i]] - t[tmp2[i-1]]))*(1 - a/b)
      segments(t[tmp2[i-1]], tmp3, t[tmp2[i]], tmp3, lwd = 2, col = "red")
    }
  }
}
