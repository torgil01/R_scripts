mkScatterPlot <- function(dat,X.var,Y.var,X.lab,Y.lab,Title,yscale=1,regFactor=FALSE){
# Make pretty scatterplots
# Input is data frame and name of X and Y variables to plot
require(ggplot2)
require(scales)

x <- dat[[X.var]]
y <- dat[[Y.var]]
# fixed limits on x-axis
x.lim <-c(-2,3)

theme_set(theme_bw(24))

# If regFactor=False, we plot a fit line for all data combine
# otherwise a fit line for each factor will be added
if (regFactor == FALSE) {
	# linear regression on whole dataset
	m <- lm(y ~ x)
	# where to place the text 
	p.x <- max(x)
	preds <- data.frame(x=p.x)
	label.y <- predict(m,newdata=preds)
	label.x <- p.x*0.75
	r2 <- signif(summary(m)$r.squared, digits = 2)
	textlab <- paste("r^2 ==", r2, sep="")
	# main plot command
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
		scale_x_continuous(limits = x.lim) +
		scale_y_continuous(labels=function(x)format(x*yscale,nsmall=2))
}
else {
	# linear regression on each factor
	m.con <- lm(y ~ x, subset=(dat$Group == '1'))
	m.ad <- lm(y ~ x, subset=(dat$Group == '2'))
	# where to place the text 
	p.x <- tapply(x,dat$Group, max)
	preds.con <- data.frame(x=p.x[1])
	preds.ad <- data.frame(x=p.x[2])
	label.y.con <- predict(m.con,newdata=preds.con)
	label.x.con <- p.x[1]*0.75
	label.y.ad <- predict(m.ad,newdata=preds.ad)
	label.x.ad <- p.x[2]*0.75
	r2.con <- signif(summary(m.con)$r.squared, digits = 2)
	r2.ad <- signif(summary(m.ad)$r.squared, digits = 2)
	textlab.con <- paste("r^2 ==", r2.con, sep="")
	textlab.ad <- paste("r^2 ==", r2.ad, sep="")
	
	# main plot command
	sc.plot <- ggplot(dat, aes_string(x = X.var, y = Y.var , colour= "Group")) +
	geom_point(size = 4)
	sc.plot + stat_smooth(method= lm, se=FALSE) +  
	xlab(X.lab) + ylab(Y.lab) + 
	annotate("text", x = label.x.con, y = label.y.con, label = textlab.con, size = 5,parse=TRUE) +
	annotate("text", x = label.x.ad, y = label.y.ad, label = textlab.ad, size = 5,parse=TRUE) +
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
	scale_x_continuous(limits = x.lim) +
	scale_y_continuous(labels=function(x)format(x*yscale,nsmall=2))
}
}

