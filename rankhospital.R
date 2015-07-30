rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
        sub_measures <- care_measures[,c(2, 7, 11, 17, 23)]
        pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        # Check that state and outcome are valid
        if (!(state %in% sub_measures[,2])) {
                stop('invalid state')
                
        } else if (!(outcome %in% pos_outcomes)){
                stop('invalid outcome')
        } else {
                if (outcome ==  pos_outcomes[1]){
                        col <- 3
                } else if (outcome ==  pos_outcomes[2]){
                        col <- 4       
                } else if (outcome ==  pos_outcomes[3]) {
                        col <- 5    
                }
                
                state_data <- subset(sub_measures, sub_measures$State == state)
                sub_state_data <- subset(state_data, 
                                         (state_data[,col] != "Not Available"))
                
                ordered<-sub_state_data[order(as.numeric(sub_state_data[,col]), (sub_state_data[,1])),]
                

                
                if (num == "best"){
                        res <- ordered[1,1] 
                                                    
                } else if (num == "worst") {
                        res <- ordered[nrow(ordered),1]
                } else {
                        if (num > nrow(sub_state_data)){
                                res <- NA
                        }else {
                                res <- ordered[num, 1]
                        }
                }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        }
        res
}
