corr <- function(directory, threshold=0){
	cor_results <- numeric(0)
	
	complete_cases <- complete(directory)
	target <- complete_cases[complete_cases$nobs >= threshold, ]
	
	if (nrow(target) > 0){
		for (monitor in target$id){
			path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
			monitor_data <- read.csv(path)
			nit <- monitor_data[(!is.na(monitor_data$nitrate)), ]
			nit <- nit[(!is.na(nit$sulfate)), ]
			sulf_data <- nit["sulfate"]
			nit_data <- nit["nitrate"]
			cor_results <- c(cor_results, cor(sulf_data, nit_data))
		}
	}
	cor_results
}