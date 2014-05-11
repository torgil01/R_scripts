calcICC <- function(dat){
# calculate the ICC between the manual segmneted ICC and the various ICC estimates
# use a two-way mixed model with absolute agreement = ICC(3,1)

# vars
  icv.est.names <- c("FS_orig","FS_opt","newRBMT1034","newRBMT1T2029",
                   "ARBMT1","ARBMT1T2")      
  icv.manual <- dat$Manual
  icc.array <- c(length(icv.est.names))
  for (k in 1:length(icv.est.names)){
    icv.est <- dat[[icv.est.names[k]]]
    icv.comp <- data.frame(icv.manual,icv.est)
    icc.results <- ICC(icv.comp)$results
    icc.array[k] <- icc.results$ICC[3] # this is ICC(3,1)
  }
  summary.dat <- data.frame(icv.est.names,icc.array)
  print(summary.dat)
}