CalcSampleSize <- function(dat,ctype){
# function for calculating required sample size from a 
# simple linear regression model: vol = a + b*age 
# Volumes are either (1) raw volumes, (2) volumes corrected by FreeSurfer ICV
# and (3) volumes corrected by RBM 
# There are two correction methods, ratio (ctype = "rat") and 
# residuals (ctype = "res")

require(pwr)
# defaults for power calc
alpha <- 0.05
power <- 0.8

# parse arg
if (ctype == "res") {
  # residual correction
}else if (ctype == "rat"){
    # ratio correction
}else {
  stop("Unknown input option")}

# function for returning p-value from regression model 
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

volumes <- c("CerebralWhiteMatter",            
             "CerebralCortex",
             "CerebellumWhiteMatter",
             "CerebellumCortex",
             "ThalamusProper",
             "Caudate",
             "Putamen",
             "Hippocampus",
             "Pallidum",
             "Amygdala",
             "Accumbensarea",
             "LateralVentricle",
             "InfLatVent")

p.raw <- c(1:length(volumes))
p.fs <- c(1:length(volumes))
p.rbm <- c(1:length(volumes))
mean.raw <- c(1:length(volumes))
mean.fs <- c(1:length(volumes))
mean.rbm <- c(1:length(volumes))
sd.raw <- c(1:length(volumes))
sd.fs <- c(1:length(volumes))
sd.rbm <- c(1:length(volumes))
rsq.raw <- c(1:length(volumes))
rsq.fs <- c(1:length(volumes))
rsq.rbm <- c(1:length(volumes))
n.raw <- c(1:length(volumes))
n.fs <- c(1:length(volumes))
n.rbm <- c(1:length(volumes))

for (k in 1:length(volumes)){
  vol <- dat[[volumes[k]]]
  age <- dat$Age
  # raw volumes, calc. ss on uncorrected volumes
  mean.raw[k] <- mean(vol)
  sd.raw[k] <- sd(vol)
  reg.raw <- lm(vol ~ age)
  p.raw[k] <- lmp(reg.raw)  
  rsq.raw[k] <- summary(reg.raw)$r.squared
  r = sqrt(rsq.raw[k])
  n.raw[k] <- pwr.r.test(n=NULL,r=r,sig.level=alpha,power=power)$n
  
  # FS icv 
  icv <- dat$FS_opt
  # what kind of correction?
  if (ctype == "res"){
    resid <- lm(vol ~ icv)
    cvol <- residuals(resid)
  }else{
    cvol <- vol/icv}
  # summary stats
  mean.fs[k] <- mean(cvol)
  sd.fs[k] <- sd(cvol)
  # regression vol ~ age
  reg.fs <- lm(cvol ~ age)
  p.fs[k] <- lmp(reg.fs) 
  rsq.fs[k] <- summary(reg.fs)$r.squared
  r = sqrt(rsq.fs[k])
  n.fs[k] <- pwr.r.test(n=NULL,r=r,sig.level=alpha,power=power)$n

  # RBM residuals
  icv <- dat$ARBM # RBMopt_03999
  # what kind of correction?
  if (ctype == "res"){
    resid <- lm(vol ~ icv) 
    cvol <- residuals(resid)
  }else{
    cvol <- vol/icv}
  # summary stats
  mean.rbm[k] <- mean(cvol)
  sd.rbm[k] <- sd(cvol)
  # regression vol ~ age
  reg.rbm <- lm(cvol ~ age)
  p.rbm[k] <- lmp(reg.rbm) 
  rsq.rbm[k] <- summary(reg.rbm)$r.squared
  r = sqrt(rsq.rbm[k])
  n.rbm[k] <- pwr.r.test(n=NULL,r=r,sig.level=alpha,power=power)$n
}
pvalues.dat <- data.frame(volumes,p.raw,p.fs,p.rbm)
summary.dat <- data.frame(volumes,mean.raw,sd.raw,mean.fs,sd.fs,mean.rbm,sd.rbm)
pwr.dat <- data.frame(volumes,rsq.raw,rsq.fs,rsq.rbm,n.raw,n.fs,n.rbm)
print(pvalues.dat,digits=2)
print(summary.dat)
print(pwr.dat) # digits = 2
}
