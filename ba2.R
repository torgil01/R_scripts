ba2.plot <- function(m1,m2,n1,n2,title="", xlab="Average (ml)", ylab="Difference (ml)")
{
  
  d1 <- ((m1 + m2)/2)
  diff1 <- m1 - m2         
  ylim <- c(-250,250)
  plot(diff1 ~ d1,pch=17,cex=0.7,ylim=ylim, xlab=xlab,ylab=ylab,col="blue",main=title)
  abline(h=mean(diff1)-c(-2,2)*sd(diff1),lty=2,col="lightskyblue")
  
  # add linear plot between difference(y) and mean (x)
  res.reg <- lm(diff1 ~ d1)
  abline(res.reg,col="lightskyblue") 
  
  d2 <- ((n1 + n2)/2)
  diff2 <- n1 - n2         
  points(diff2 ~ d2,pch=20,cex=0.7, col="red")
  abline(h=mean(diff2)-c(-2,2)*sd(diff2),lty=2,col="pink")
  
  # add linear plot between difference(y) and mean (x)
  res.reg <- lm(diff2 ~ d2)
  abline(res.reg,col="pink") 
  
  
  }
  