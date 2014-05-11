compare.manualseg <- function(data,vars){
# compute %DIFF and absolute % diff between manual segmentation volumes and estimated ICV
  

N <- length(vars)
Nsubj <- length(data)
diff.frame <- data.frame(sex=character(Nsubj),icv.diff=numeric(Nsubj),icv.adiff=numeric(Nsubj))

diffstats.all <- data.frame(method=character(N), mean=numeric(N), std=numeric(N),
                            amean=numeric(N), astd=numeric(N),
                         minval=numeric(N), maxval=numeric(N), stringsAsFactors=FALSE)
diffstats.males <- data.frame(method=character(N), mean=numeric(N), std=numeric(N),
                              amean=numeric(N), astd=numeric(N), stringsAsFactors=FALSE)
diffstats.females <- data.frame(method=character(N), mean=numeric(N), std=numeric(N),
                              amean=numeric(N), astd=numeric(N), stringsAsFactors=FALSE)
stat.frame <- data.frame(method=character(N), t.diff=numeric(N), p.diff=numeric(N)
                         , t.adiff=numeric(N), p.adiff=numeric(N),stringsAsFactors=FALSE)

# stupid fix because of different coding of sex in hunt and tos datasets
# tos (sex= 0 (female) sex = 1 (male)); hunt(sex = 1 (female) sex = 2 (male))
male <- max(data$Sex)
female  <-  min(data$Sex)

manual <- data$Manual
sex <- data$Sex
# stats for whole dataset
  for (k in 1:length(vars)){
    estimate <- data[[vars[k]]]   
    icv.diff <- manual - estimate
    icv.adiff <- abs(manual-estimate)
   
    # add to data frame
    diff.frame <- data.frame(sex,icv.diff,icv.adiff)
    
    diff.mean <- mean(icv.diff)
    diff.std <- sd(icv.diff)
    diff.amean <- mean(icv.adiff)
    diff.astd <- sd(icv.adiff)
    diff.min <- min(icv.diff)
    diff.max <- max(icv.diff)
    
    
    
    # add row to dataframe
    diffstats.all[k,] <- list(vars[k],diff.mean,diff.std,diff.amean,diff.astd,diff.min,diff.max)
    
    # diffs for males/females separately 
    mean.sex <- tapply(diff.frame$icv.diff,diff.frame$sex,mean)
    amean.sex <- tapply(diff.frame$icv.adiff,diff.frame$sex,mean)
    std.sex <- tapply(diff.frame$icv.diff,diff.frame$sex,sd)
    astd.sex <- tapply(diff.frame$icv.adiff,diff.frame$sex,sd)
    
    # add row to dataframe
    diffstats.males[k,] <- list(vars[k],mean.sex[2],std.sex[2],amean.sex[2],astd.sex[2])
    diffstats.females[k,] <- list(vars[k],mean.sex[1],std.sex[1],amean.sex[1],astd.sex[1])
    
    # test whether means are sig different
    male.diff <- subset(diff.frame, Select=icv.diff, sex == male)
    female.diff <- subset(diff.frame, Select=icv.diff, sex == female)
    male.adiff <- subset(diff.frame, Select=icv.adiff, sex == male)
    female.adiff <- subset(diff.frame, Select=icv.adiff, sex == female)
    
    tt.diff <- t.test(female.diff,male.diff)
    tt.adiff <- t.test(female.adiff,male.adiff)
    stat.frame[k,] <- list(vars[k],tt.diff$statistic,tt.diff$p.value,tt.adiff$statistic,tt.adiff$p.value)    

  }

# rownames(my.data)


#sm <- data.frame(vars,means,stds,mins,maxes)
print(diffstats.all)
print(diffstats.males)
print(diffstats.females)
print(stat.frame)


}
