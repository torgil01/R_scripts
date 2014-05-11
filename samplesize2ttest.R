samplesize.2t.test <- function(dat,icv.type,delta = 0.1){
# function for calculating required sample size from a 2.sampe t.test 
# here we use the "power.t.test" from the "stats" package
# We calculate the sample size for a hypothetical difference in the mean value
# say a 10% difference from the mean, that is grup 2 have a mean that is 10% 
# lower than group 1. Since we are calculating the sample size for the raw volume, 
# the residual corrected volumes and for the ratio corrected values, we have to 
# pay attention to the scaling. The raw and residual corrected volumeas are on the same scale, 
# but the ratio corrected volumes are not. For the ratio volumes and a 10% difference in the means 
# one show that delta = mean(0.1*vol/icv)
  
# defaults for power calc
alpha <- 0.05
power <- 0.8
alternative <- "two.sided"
# these are the brain volumes we calculate the sample size estimates for 
volumes <- c("SubCort_Total_Cerebral_White_Matter",            
             "SubCort_Total_Cerebral_Cortex",
             "SubCort_Total_Cerebellum_White_Matter",
             "SubCort_Total_Cerebellum_Cortex",
             "SubCort_Total_Thalamus_Proper",
             "SubCort_Total_Caudate",
             "SubCort_Total_Putamen",
             "SubCort_Total_Hippocampus",
             "SubCort_Total_Pallidum",
             "SubCort_Total_Amygdala",
             "SubCort_Total_Accumbens_area",
             "SubCort_AllVentricles")

#              "LateralVentricle",
#              "InfLatVent") 
#             "SubCortAllVentricles")


# volumes <- c( "SubCortLeftCerebralWhiteMatter",
#               "SubCortRightCerebralWhiteMatter",
#               "SubCortLeftCerebralCortex",
#               "SubCortRightCerebralCortex",
#               "SubCortLeftCerebellumWhiteMatter",
#               "SubCortRightCerebellumWhiteMatter",
#               "SubCortLeftCerebellumCortex",
#               "SubCortRightCerebellumCortex",
#               "SubCortLeftPutamen",
#               "SubCortRightPutamen",
#               "SubCortLeftCaudate",
#               "SubCortRightCaudate",
#               "SubCortLeftPallidum",
#               "SubCortRightPallidum",
#               "SubCortLeftHippocampus",
#               "SubCortRightHippocampus",
#               "SubCortLeftAccumbensarea",
#               "SubCortRightAccumbensarea")

mean.raw <- c(1:length(volumes))
mean.resid <- c(1:length(volumes))
mean.ratio <- c(1:length(volumes))
sd.raw <- c(1:length(volumes))
sd.resid <- c(1:length(volumes))
sd.ratio <- c(1:length(volumes))
n.raw <- c(1:length(volumes))
n.resid <- c(1:length(volumes))
n.ratio <- c(1:length(volumes))

for (k in 1:length(volumes)){
  vol <- dat[[volumes[k]]]
  
  # raw volumes, calc. ss on uncorrected volumes
  mean.raw[k] <- mean(vol)
  sd.raw[k] <- sd(vol)
  diff <- delta*mean(vol)
  n.raw[k] <- power.t.test(n=NULL, delta=diff, sd=sd(vol),sig.level=alpha, power=power,
                           alternative = alternative,type="two.sample")$n
  
  icv <- dat[[icv.type]]# the hunt_dataset have icv in ml
  #icv <- icv + rnorm(length(icv),mean=0,sd=99995398)
  # ICV residual corerction 
  icv.mc <- icv - mean(icv)
  resid <- lm(vol ~ icv.mc)
  cvol <- residuals(resid) + mean(vol)  # we add the mean back 
  mean.resid[k] <- mean(cvol)  # this will be the same as for the raw 
  sd.resid[k] <- sd(cvol)
  diff <- delta*mean.resid[k]
  n.resid[k] <- power.t.test(n=NULL, delta=diff, sd=sd.resid[k],sig.level=alpha, power=power,
                           alternative = alternative,type="two.sample")$n
  
  # ICV ratio correrction 
  cvol <- vol/icv   
  mean.ratio[k] <- mean(cvol)  # this will be the same as for the raw 
  sd.ratio[k] <- sd(cvol)
  diff <- mean(delta*cvol)
  n.ratio[k] <- power.t.test(n=NULL, delta=diff, sd=sd.ratio[k],sig.level=alpha, power=power,
                           alternative = alternative,type="two.sample")$n
}

summary.dat <- data.frame(volumes,mean.raw,sd.raw,mean.resid,sd.resid,mean.ratio,sd.ratio)
pwr.dat <- data.frame(volumes,n.raw,n.resid,n.ratio)
print(summary.dat)
print(pwr.dat,digits=3) # digits = 2
}
