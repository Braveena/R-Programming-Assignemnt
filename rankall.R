rankall <- function(outcome, num = "best") {
        ## Read outcome data
        care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
        sub_measures <- care_measures[,c(2, 7, 11, 17, 23)]
        pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
        final_res <- data.frame(character(), character())
        
        ## Check that outcome are valid
        if (!(outcome %in% pos_outcomes)){
                stop('invalid outcome')
        } else {
                ## For each state, find the hospital of the given rank
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                
                if (outcome ==  pos_outcomes[1]){
                        col <- 3
                } else if (outcome ==  pos_outcomes[2]){
                        col <- 4       
                } else if (outcome ==  pos_outcomes[3]) {
                        col <- 5    
                }
                
                state_no <- length(unique(sub_measures$State))
                
                all_state <- unique(sub_measures$State)
                
                for(i in 1:state_no){
                        state_data <- subset(sub_measures, sub_measures$State 
                                             == all_state[i])
                        sub_state_data <- subset(state_data, 
                                                 (state_data[,col] 
                                                  != "Not Available"))
                        
                        ordered<-sub_state_data[order
                                                (as.numeric(sub_state_data[,col]), 
                                                (sub_state_data[,1])),]
                        
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
                        
                        final_res <- rbind(final_res, cbind(res, as.character(all_state[i])))
                        
                }
                
        }
        row.names(final_res) <- final_res[,2]
        final_res<-final_res[order(as.character(final_res[,2])),]
        colnames(final_res)<-c("hospital", "state")
        final_res
        

}