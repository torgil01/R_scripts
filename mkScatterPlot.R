mkScatterPlot <- function(dat,X.var,Y.var,X.lab,Y.lab,Title,yscale=1){
# Make pretty scatterplots
# Input is data frame and name of X and Y variables to plot
require(ggplot2)
require(scales)

x <- dat[[X.var]]
y <- dat[[Y.var]]

m <- lm(y ~ x)
a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
r2 <- signif(summary(m)$r.squared, digits = 2)
textlab <- paste("r^2 ==", r2, sep="")
#print(textlab)
theme_set(theme_bw(24))
# where to place the text 
p.x <- max(x)*0.92
preds <- data.frame(x=p.x)
label.y <- predict(m,newdata=preds)*0.98
label.x <- p.x
sc.plot <- ggplot(dat, aes_string(x = X.var, y = Y.var , colour= "Group")) +
 geom_point(size = 4)
sc.plot + geom_abline(intercept = a, slope = b, colour = "blue", size = 1) +  
	xlab(X.lab) + ylab(Y.lab) + 
	annotate("text", x = label.x, y = label.y, label = textlab, size = 5,parse=TRUE) +
	ggtitle(Title) + 
	theme(legend.title=element_blank()) +  
	theme(legend.key = element_blank(), panel.border = element_blank()) +
	theme(axis.line = element_line(color = 'black'), 
				axis.text.x  = element_text(size=14), 
				axis.text.y  = element_text(size=14),
				axis.title.x  = element_text(size=16),
				axis.title.y = element_text(size=16)) +
	scale_color_discrete(breaks=c("1", "2"),
											labels=c("Control", "AD")) + 
	scale_y_continuous(labels=function(x)format(x*yscale,nsmall=2))
}

