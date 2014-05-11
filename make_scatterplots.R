# script for making scatterplots

#mkScatterPlot(AD,"PH_ACC","FA_ALL_PHACC","Phonemic accuracy (Z-score)", "Mean FA", "Phonemic Accuracy vs. FA")
#AD <- transform(AD, MD_ALL_PHACC = MD_ALL_PHACC*1000)
Y.lab <- expression(paste('Mean MD [',10^-3,'*',mm^2,'/s]'))
mkScatterPlot(AD,"PH_ACC","MD_ALL_PHACC","Phonemic accuracy (Z-score)", 
							Y.lab, "Phonemic Accuracy vs. MD",yscale=1000)