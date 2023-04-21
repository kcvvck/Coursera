rankall <- function(outcome, num = "best"){
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
	out <- paste("Hospital.30.Day.Death..Mortality..Rates.from." , outcome, sep = "")
	o <- read.csv("outcome-of-care-measures.csv")
	# get unique states and sort alphabetically 
	u <- sort(unique(o$State))
	# u is a list
	# create empty dataframe
	my_data <- data.frame()
	# set up a counter to keep list index later
	count <- 1
	# tedious, brute force method using loop
	for (i in u){
		# filter out other states
		temp_data <- o[which(o$State == u[count]), ]
		# subset data to just keep hosp.name and outcome
		temp_data <- temp_data[, c("Hospital.Name", out)]
		# convert to numeric so I can order later
		temp_data[,2] <- suppressWarnings(as.numeric(temp_data[,2]))
		# order it using 2 columns
		temp_data <- temp_data[order(temp_data[,2], temp_data[,1], na.last = NA), ]
		# adding a rank:: NOT NECESSARY
		temp_data$Rank <- as.numeric(seq.int(nrow(temp_data)))
		# put all the data I want into a new dataframe
		if (num == "best"){
			my_data <- rbind(my_data, c(temp_data[1,1], u[count]))
		}else if (num =="worst"){
			my_data <- rbind(my_data, c(temp_data[nrow(temp_data), 1], u[count]))
		}else{
			my_data <- rbind(my_data, c(temp_data[num, 1], u[count]))
		}
		count <- count + 1
	}
	# set headers as hosp and state
	names(my_data) <- c("hospital", "state")
	my_data

}