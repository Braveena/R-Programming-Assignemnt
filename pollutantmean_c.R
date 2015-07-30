pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        # saves all the files in directory folder
        files <- list.files(directory)
        
        # creates an empty dataframe to hold the data read
        dat <- data.frame()
        
        # reads data from
        
        for (i in 1:id) {
                dat <- rbind(dat, read.csv(files[i]))
        }
        
        if (pollutant == "sulfate"){
                col_id <- 2
        } else if (pollutant == "nitrate") {
                col_id <-3
        }
        colmean <- mean(dat[ ,col_id], na.rm=TRUE)
}
        