corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        comp_files <- complete(directory)
        no_comp_files <- nrow(comp_files)
        
        files <- list.files(directory, full.names=TRUE)
        
        all_cor <- vector(mode="numeric")
        for (i in 1:no_comp_files){
                if (comp_files[i,2] > threshold) {
                        dat <- read.csv(files[comp_files[i,1]], "header" = TRUE)
                        good <- complete.cases(dat)
                        cal <- dat[good, ][, 1:4]
                        corr <- cor(cal[,2], cal[,3])
                        all_cor <- c(all_cor, corr)
                }
        }
        all_cor
        
        
}