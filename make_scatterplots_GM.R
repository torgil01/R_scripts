# script for making scatterplots for GM density

# 
filename <- "GM_SE_ACC.pdf"
mkScatterPlot(AD,"SE_ACC","GM_ALL_SEACCINTR","Semantic accuracy (Z-score)",
							"Mean GM density", "GM density vs. Semantic Accuracy",regFactor=TRUE)
mySave2pdf(filename)