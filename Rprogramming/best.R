best <- function(state, outcome){
	if (outcome == "heart attack")
	{
		outcome <- "Heart.Attack"
	}else if (outcome == "heart failure"){
		outcome <- "Heart.Failure"
	}else{
		outcome <- "Pneumonia"
	}
	out <- paste("Hospital.30.Day.Death..Mortality..Rates.from." , outcome, sep = "")
	filer <- read.csv("outcome-of-care-measures.csv")
	# need to subset data 
	filer1 <- filer[which(filer$State == state), ]
	# because they want specific states

	interested_data <- filer1[, c(out, "Hospital.Name")]
	interested_data[,1] <- as.numeric(interested_data[,1])
	interested_data <-interested_data[order(interested_data$Hospital.Name), ]
	output <- interested_data[which.min(interested_data[, out]),2]
	output
}