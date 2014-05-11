doFactor <- function(dat){
	# function for doing factor analysis on AD data 
	#	we read in the whole dataset and extract new dataframe with the 
	# essensial data for FA
	
	# required libs
	require(psy)
	require(nFactors)
	
	# set up vector of variables to include in the PCA
	# just to be on the safe side we take the raw scores
	# and do an z-transform afterwards
	vf.vars <- c("F_korrekte",
							 "A_korrekte",
							 "S_korrekte",
							 "Dyr_korrekte",
							 "Frukt_korrekte",
							 "Yrke_korrekte",
							 "Median_int_F",
							 "Median_int_A",
							 "Median_int_S",
							 "Median_int_DYR",
							 "Median_int_FRUKT",
							 "Median_int_YRKE")
	
#	vf.vars <- c("PH_ACC","PH_INT","PH_SD","SE_ACC","SE_INT","SE_SD")
	
	
	# make data frame w. PCA vars
	vf.data <- dat[vf.vars]
	vf.data <- na.omit(vf.data)
	# z transform
	vf.data[] <- lapply(vf.data,scale)

	#summary(na.omit(vf.data))
	
	# scree
#	scree.plot(vf.data,type="R")
	
  # FA
	#vf.factor <- factanal(vf.data, 4, rotation="varimax", scores="regression",na.action=na.omit)
	vf.factor <- factanal(~.,data=vf.data, 2, rotation="varimax",na.action=na.omit)
	summary(vf.factor)	
	print(vf.factor, digits=2, cutoff=.5, sort=FALSE)
	# plot factor 1 by factor 2 
	load <- vf.factor$loadings[,1:2] 
	plot(load,type="n") # set up plot 
	text(load,labels=names(vf.data),cex=.7) # add variable names
	
	# Determine Number of Factors to Extract

	ev <- eigen(cor(vf.data)) # get eigenvalues
	ap <- parallel(subject=nrow(vf.data),var=ncol(vf.data),
								 rep=100,cent=.05)
	nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
	plotnScree(nS)
	
	
}