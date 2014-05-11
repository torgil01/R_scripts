mySave2pdf <- function(filename){
	dev.copy2pdf(file=filename, bg="white",height=6) 
}