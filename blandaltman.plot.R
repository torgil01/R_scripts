blandaltman.plot <- function(m1,m2, title="", xlab="Average", ylab="Difference")
{
  
  d <- ((m1 + m2)/2)
  diff <- m1 - m2         
  ylim <- c(-250,250)
  plot(diff ~ d,pch=17,cex=0.7,ylim=ylim, xlab=xlab,ylab=ylab,col="blue",main=title)
  #abline(h=mean(diff)-c(-2,0,2)*sd(diff),lty=2,col="lightskyblue")
  abline(h=mean(diff)-c(-2,2)*sd(diff),lty=2,col="lightskyblue")
  
  # add linear plot between difference(y) and mean (x)
  res.reg <- lm(diff ~ d)
  abline(res.reg,col="lightskyblue") 
}