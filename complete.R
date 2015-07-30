complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        
        files <- list.files(directory, full.names=TRUE)
        
        comp_dat <- data.frame()

        for (i in id) {
                dat <- read.csv(files[i])
                good <- complete.cases(dat)
                comp_dat <- rbind(comp_dat, c(i, nrow(dat[good, ][, 1:4])))
                
        }
        heading <- c('id', 'nobs')
        colnames(comp_dat) <- heading
        
        comp_dat
}