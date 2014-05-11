CalcICV.corr<- function(dat,icv.type){
  # Calculate the Pearson's correlation between brain volume measurements
  # and ICV
  
  
  # The volumes
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
  
  # Pearsons' r for volume ~ ICV
  r.array <- c(1:length(volumes))
  icv <- dat[[icv.type]]
  
  # loop over variables
  for (k in 1:length(volumes)){
    vol <- dat[[volumes[k]]]
    r.array[k] <- cor(vol,icv)
  }
  summary.dat <- data.frame(volumes,r.array)
  print(summary.dat)
}
