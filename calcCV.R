CalcCV <- function(dat,icv.type){
  # Calculate the "coefficient of variation" (CV) for a range of brain volume measures 
  # Volumes are either (1) raw volumes, (2) volumes corrected by FreeSurfer ICV
  # and (3) volumes corrected by RBM 
  # There are two correction methods, ratio (ctype = "rat") and 
  # residuals (ctype = "res")
  
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
               "InfLatVent",
               "SubCortAllVentricles")

  #
  
  
  # array of mean values for each volume 
  mean.raw <- c(1:length(volumes))
  mean.ratio <- c(1:length(volumes))
  mean.resid <- c(1:length(volumes))
  # std of each volume 
  sd.raw <- c(1:length(volumes))
  sd.ratio <- c(1:length(volumes))
  sd.resid <- c(1:length(volumes))
  # r^2 for volume ~ ICV
  r.raw <- c(1:length(volumes))
  r.ratio <- c(1:length(volumes))
  r.resid <- c(1:length(volumes))
  icv <- dat[[icv.type]]
  #icv <- dat$FS_orig
  icv.mc <- icv - mean(icv)
  # loop over variables
  for (k in 1:length(volumes)){
    vol <- dat[[volumes[k]]]

    # raw volumes, calc. ss on uncorrected volumes
    mean.raw[k] <- mean(vol)
    sd.raw[k] <- sd(vol)
    r.raw[k] <- cor(vol,icv)
    
    # ratio  
    cvol <- vol/icv
    mean.ratio[k] <- mean(cvol)
    sd.ratio[k] <- sd(cvol)
    r.ratio[k] <- cor(cvol,icv)
    # residuals
    resid <- lm(vol ~ icv.mc) # note mean corrected icvs 
    cvol <- residuals(resid) + mean(vol) # note add mean tol resid
    mean.resid[k] <- mean(cvol)
    sd.resid[k] <- sd(cvol)
    r.resid[k] <- cor(cvol,icv)
    }
  
  # print summaries
  cv.raw <- sd.raw/mean.raw
  cv.ratio <- sd.ratio/mean.ratio
  cv.resid <- sd.resid/mean.resid
  summary.dat <- data.frame(volumes,cv.raw,r.raw,cv.ratio,r.ratio,cv.resid,r.resid)
  print(summary.dat)
}
