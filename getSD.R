SummarizeVolumes <- function(dat,ctype)
# print summary of raw and ICV correctd volunes from HUNT/TOS dataset
# code written so to avoid needing to attach the dataframe 
# the input is the dataframe "dat" containing the volumes from the 
# TOS/HUNT datasets

  
  
volumes <- c("CerebralWhiteMatter"   ,            
"CerebralCortex", 
"LateralVentricle",                  
"InfLatVent",                 
"CerebellumWhiteMatter",             
"CerebellumCortex",           
"ThalamusProper",                 
"Caudate",                          
"Putamen",                           
"Pallidum",                         
"Hippocampus",                       
"Amygdala",                         
"Accumbensarea",                       
"SubCortBrainStem",  
"SubCortWholeBrain",
"SubCort3rdVentricle", 
"SubCortCSF",
"SubCort4thVentricle")

sd_raw <- c(1:length(volumes))
sd_fs <- c(1:length(volumes))
sd_rbm <- c(1:length(volumes))
mean_raw <- c(1:length(volumes))
mean_fs <- c(1:length(volumes))
mean_rbm <- c(1:length(volumes))
#
for (k in 1:length(volumes)){
  vol <- dat[[volumes[k]]]
  #raw data 
  mean_raw <- mean(vol)
  sd_raw <- sd(vol)
  # FS residuals
  icv <- dat[[volumes[k]]]
  
  reg_fs <- lm(vol ~ FS_orig)
  res_fs <- residuals(reg_fs)
  sd_fs[k] <- sd(res_fs)
  #m <- mean(res_fs)
  cc <- coef(reg_fs)
  a <- cc[1]
  b <- cc[2]
  # predict icv from linear model
  icv <- ((mvol + eff*mvol) -a)/b
  # calculate residual for this icv
  resid <- (mvol + eff*mvol) -a -b*icv
  mx <-0
  my <- resid
  d_fs[k] <- cohens_d(mx,my,sd_fs[k])
  # RBM residuals
  reg_rbm <- lm(vol ~ RBM_T02965)
  res_rbm <- residuals(reg_rbm)
  sd_rbm[k] <- sd(res_rbm)   
  m <- mean(res_rbm)
  mx = m
  my = m + eff*m
  d_rbm[k] <- cohens_d(mx,my,sd_rbm[k])  
 }
mydat <- data.frame(volumes,sd_fs,sd_rbm,d_fs,d_rbm)
#tbl <- table(volumes,sd_fs)
#print(tbl)
print(mydat)

# quasi function for calculation Cohen's
# "quasi" because we just take the mean as input
cohens_d <- function(mx, my,sd) {
  md  <- abs(mx - my)        ## mean difference (numerator)
  cd  <- md/sd               ## cohen's d
}
