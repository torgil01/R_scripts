# script for making scatterplots

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


