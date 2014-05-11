
qcd <- function(dat){
#  compute the "Quartile coefficient of dispersion" for vector dat
  
  q <- quantile(dat)
  QQ <- (q[4] - q[2])/(q[4] + q[2])
  return(QQ)
}