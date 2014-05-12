# script for making scatterplots on DTI indices

# FA PH_ACC
filename <- "FA_PH_ACC.pdf"
mkScatterPlot(AD,"PH_ACC","FA_ALL_PHACC","Phonemic accuracy (Z-score)",
							"Mean FA", "FA vs. Phonemic Accuracy")
mySave2pdf(filename)

# MD PH_ACC
filename <- "MD_PH_ACC.pdf"
graphics.off()
Y.lab <- expression(paste('Mean MD [',10^-3,'*',mm^2,'/s]'))
mkScatterPlot(AD,"PH_ACC","MD_ALL_PHACC","Phonemic accuracy (Z-score)", 
							Y.lab, "MD vs. Phonemic Accuracy",yscale=1000)
mySave2pdf(filename)

# FA SE_ACC
filename <- "FA_SE_ACC.pdf"
graphics.off()
mkScatterPlot(AD,"SE_ACC","FA_ALL_SEACC","Semantic accuracy (Z-score)",
							"Mean FA", "FA vs. Semantic Accuracy")
mySave2pdf(filename)

# MD SE_ACC
filename <- "MD_SE_ACC.pdf" 
graphics.off()
.lab <- expression(paste('Mean MD [',10^-3,'*',mm^2,'/s]'))
mkScatterPlot(AD,"SE_ACC","MD_ALL_SEACC","Semantic accuracy (Z-score)",Y.lab,
							"MD vs. Semantic Accuracy",yscale=1000)
mySave2pdf(filename)

##############
# FA SE_INT
filename <- "FA_SE_INT.pdf"
graphics.off()
mkScatterPlot(AD,"SE_INT","FA_ALL_SEINT","Semantic intervals (Z-score)",
							"Mean FA", "FA vs. Semantic Intervals")
mySave2pdf(filename)

# MD PH_INT
filename <- "MD_PH_INT.pdf" 
graphics.off()
Y.lab <- expression(paste('Mean MD [',10^-3,'*',mm^2,'/s]'))
mkScatterPlot(AD,"PH_INT","MD_ALL_PHINT","Phonemic intervals (Z-score)",Y.lab,
							"MD vs. Phonemic Intervals",yscale=1000)
mySave2pdf(filename)

# MD SE_ACC
filename <- "MD_SE_INT.pdf" 
graphics.off()
Y.lab <- expression(paste('Mean MD [',10^-3,'*',mm^2,'/s]'))
mkScatterPlot(AD,"SE_INT","MD_ALL_SEINT","Semantic intervals (Z-score)",Y.lab,
							"MD vs. Semantic Intervals",yscale=1000)
mySave2pdf(filename)

