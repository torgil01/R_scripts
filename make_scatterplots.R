# script for making scatterplots

dev.copy(file="FA_PH_ACC.pdf",device=pdf, bg="white") 
graphics.off()

mkScatterPlot(AD,"PH_ACC","FA_ALL_PHACC","Phonemic accuracy (Z-score)", "Mean FA", "Phonemic Accuracy vs. FA")
ggsave("FA_PH_ACC.pdf")
Y.lab <- expression(paste('Mean MD [',10^-3,'*',mm^2,'/s]'))
mkScatterPlot(AD,"PH_ACC","MD_ALL_PHACC","Phonemic accuracy (Z-score)", 
							Y.lab, "Phonemic Accuracy vs. MD",yscale=1000)