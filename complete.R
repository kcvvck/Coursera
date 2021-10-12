complete <- function(directory, id = 1:332){
 	results <- data.frame(id = numeric(0), nobs = numeric(0))
 	for (monitor in id){
 		path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
 		monitor_data <- read.csv(path)
 		nit <- monitor_data[(!is.na(monitor_data$nitrate)), ]
 		sulf <- nit[(!is.na(nit$sulfate)), ]
 		nobs <- nrow(sulf)
 		results <- rbind(results, data.frame(id = monitor, nobs = nobs))
 		print(results)
 		
 	}
 	results
 }