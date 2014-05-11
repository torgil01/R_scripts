summarize.frame <- function(data,vars){
  # summarize (mean, std ..) of certain variables in data frame "data"
  # the variables to be summarized are given in vector "vars"

# preallocate data frames
N <- length(vars)
sstats.all <- data.frame(method=character(N), mean=numeric(N), std=numeric(N),
                          minval=numeric(N), maxval=numeric(N), stringsAsFactors=FALSE)
sstats.males <- data.frame(method=character(N), mean=numeric(N), std=numeric(N),
                         minval=numeric(N), maxval=numeric(N), stringsAsFactors=FALSE)

sstats.females <- data.frame(method=character(N), mean=numeric(N), std=numeric(N),
                         minval=numeric(N), maxval=numeric(N), stringsAsFactors=FALSE)


# stats for whole dataset
  for (k in 1:length(vars)){
    stat <- data[[vars[k]]]
    means <- mean(stat)
    stds <- sd(stat)
    mins <- min(stat)
    maxes<- max(stat)
    # add row to dataframe
    sstats.all[k,] <- list(vars[k],means,stds,mins,maxes)
    
    # males and females
    means.sex <- tapply(data[[vars[k]]],data$Sex,mean)
    stds.sex <- tapply(data[[vars[k]]],data$Sex,sd)
    mins.sex <- tapply(data[[vars[k]]],data$Sex,min)
    maxes.sex<- tapply(data[[vars[k]]],data$Sex,max)
    male.row <- list(vars[k],means.sex[2],stds.sex[2],mins.sex[2],maxes.sex[2])
    female.row <- list(vars[k],means.sex[1],stds.sex[1],mins.sex[1],maxes.sex[1])
    sstats.males[k,] <- male.row
    sstats.females[k,] <- female.row
  }

# rownames(my.data)


#sm <- data.frame(vars,means,stds,mins,maxes)
print(sstats.all)
print(sstats.males)
print(sstats.females)

}
