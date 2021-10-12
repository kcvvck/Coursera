rankhospital <- function(state, outcome, num = "best"){
	o <- read.csv("outcome-of-care-measures.csv")
	# changing the names to fit those in column so I can find them
	if (outcome == "heart attack")
	{
		outcome <- "Heart.Attack"
	}else if (outcome == "heart failure"){
		outcome <- "Heart.Failure"
	}else if (outcome == "pneumonia"){
		outcome <- "Pneumonia"
	}else{
		# outside the 3 main outcomes
		stop("invalid outcome")
	}
	if (state %in% unique(o[, "State"])){
		out <- paste("Hospital.30.Day.Death..Mortality..Rates.from." , outcome, sep = "")
		# filter out other states
		o <- o[which(o$State == state), ]
		# subset data to just keep hosp.name and outcome
		subo <- o[, c("Hospital.Name", out)]
		# convert to numeric so I can order later
		subo[,2] <- as.numeric(subo[,2])
		# order it using 2 columns
		subo <- subo[order(subo[,2], subo[,1], na.last = NA), ]
		# adding a rank:: NOT NECESSARY
		subo$Rank <- seq.int(nrow(subo))
		if (num == "best"){
			return(subo[1,1])
		}else if (num =="worst"){
			return(subo[nrow(subo), 1])
		}else{
			return(subo[num, 1])
		}
	}else{
		stop("invalid state")
	}
}