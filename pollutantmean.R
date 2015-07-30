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
        # "full.names" argument will prepend the path to the file name
        files <- list.files(directory, full.names=TRUE)
        
        # creates an empty dataframe to hold the data read
        dat <- data.frame()
        
        # reads only subfiles denoted by id
        #rbind applied on dat append data rather than re-writing
        for (i in id) {
                dat <- rbind(dat, read.csv(files[i]))
        }
        
        # identify the appropriate column to process
        if (pollutant == "sulfate"){
                col_id <- 2
        } else if (pollutant == "nitrate") {
                col_id <-3
        }
        
      
       # calculates means while not considering NA
        colmean <- mean(dat[ ,col_id], na.rm=TRUE)
        colmean
}
        