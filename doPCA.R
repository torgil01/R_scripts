doPCA <- function(dat){
	# function for doing PCA on AD data 
	#	we read in the whole dataset and extract new dataframe with the 
	# essensial data for PCA
	
	# required libs


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
	
	
	# make data frame w. PCA vars
	vf.data <- dat[vf.vars]

	
	# first PCA wo. ztransform
	
	#vf.raw.pca <- prcomp(vf.data, scale=T)
	vf.raw.pca <- prcomp(~.,data=vf.data, scale=T, center=T,na.action=na.omit)
	summary(vf.raw.pca)
	screeplot(vf.raw.pca,npcs = 10,type="lines")
}