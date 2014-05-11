quantIntervals <- function(dat){
	# get some sense out of the interval data 
	
F.names <- c("F_intervall_1",
						 "F_intervall_2",
						 "F_intervall_3",
						 "F_interval_4",
						 "F_intervall_5",        
						 "F_intervall_6",        
						 "F_intervall_7",
						 "F_intervall_8",
						 "F_intervall_9",       
						 "F_intervall_10",
						 "F_intervall_11",
						 "F_intervall_12",
						 "F_intervall_13",
						 "F_intervall_14",
						 "F_intervall_15",
						 "F_intervall_16"
						 )	

A.names <- c("A_intervall_1",
						 "A_intervall_2",
						 "A_intervall_3",
						 "A_interval_4",
						 "A_intervall_5",        
						 "A_intervall_6",        
						 "A_intervall_7",
						 "A_intervall_8",
						 "A_intervall_9",       
						 "A_intervall_10",
						 "A_intervall_11",
						 "A_intervall_12"
							)	

S.names <- c("F_intervall_1",
						 "F_intervall_2",
						 "F_intervall_3",
						 "F_interval_4",
						 "F_intervall_5",        
						 "F_intervall_6",        
						 "F_intervall_7",
						 "F_intervall_8",
						 "F_intervall_9",       
						 "F_intervall_10",
						 "F_intervall_11",
						 "F_intervall_12",
						 "F_intervall_13",
						 "F_intervall_14",
						 "F_intervall_15"
							)

DYR.names <- c("DYR_intervall_1",
						 "DYR_intervall_2",
						 "DYR_intervall_3",
						 "DYR_interval_4",
						 "DYR_intervall_5",        
						 "DYR_intervall_6",        
						 "DYR_intervall_7",
						 "DYR_intervall_8",
						 "DYR_intervall_9",       
						 "DYR_intervall_10",
						 "DYR_intervall_11",
						 "DYR_intervall_12",
						 "DYR_intervall_13",
						 "DYR_intervall_14",
						 "DYR_intervall_15"
)

FRUKT.names <- c("FRUKT_intervall_1",
							 "FRUKT_intervall_2",
							 "FRUKT_intervall_3",
							 "FRUKT_interval_4",
							 "FRUKT_intervall_5",        
							 "FRUKT_intervall_6",        
							 "FRUKT_intervall_7",
							 "FRUKT_intervall_8",
							 "FRUKT_intervall_9",       
							 "FRUKT_intervall_10",
							 "FRUKT_intervall_11",
							 "FRUKT_intervall_12",
							 "FRUKT_intervall_13"
								)

YRKE.names <- c("YRKE_intervall_1",
								 "YRKE_intervall_2",
								 "YRKE_intervall_3",
								 "YRKE_interval_4",
								 "YRKE_intervall_5",        
								 "YRKE_intervall_6",        
								 "YRKE_intervall_7",
								 "YRKE_intervall_8",
								 "YRKE_intervall_9",       
								 "YRKE_intervall_10",
								 "YRKE_intervall_11",
								 "YRKE_intervall_12",
								 "YRKE_intervall_13",
								 "YRKE_intervall_14"
									)

# CREATE SUMMARY STATS FOR INTERVALS
# median / mean / SD / var



F.int <- dat[F.names]
# have to loop over cases here; 
F.mat <- as.matrix(F.int)
numel <- nrow(F.mat)

# allocate summary vectors
int.sd <- c(1:numel)
int.mean <- c(1:numel)
int.median <- c(1:numel)
int.mad <- c(1:numel)
for (k in 1:numel){
	v <- na.omit(F.mat[k,])
	print(k)
	int.sd[k] <- sd(v)
	int.mean[k] <- mean(v)
	int.median[k] <- median(v)
	int.mad[k] <- mad(v)
	
}

labels <- c("F.sd","F.mean","F.median","F.mad")
intervals.frame <- data.frame(F.sd=int.sd,F.mean=int.mean,
															F.median=int.median,F.mad=int.mad)
#intervals.frame <- data.frame(int.sd,int.mean,int.median,int.mad)
summary(intervals.frame)
return(intervals.frame)

}

