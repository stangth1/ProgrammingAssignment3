rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    
    oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    possible_outcomes  <- c("heart attack", "heart failure", "pneumonia")
    if (is.element(outcome,possible_outcomes) == FALSE) {
        stop("outcome not valid")
    }
    
    ## For each state, find the hospital of the given rank
    
    # get the column index based on outcome
    column_index <- data.frame(condition = c('heart attack','heart failure','pneumonia'),col_index=c(11,17,23))
    ci <- column_index$col_index[column_index$condition==outcome]
    
    df <- data.frame(hospital=character(),state=character())
    
    for (s in sort(unique(oocm$State))){
        
        #subsetting oocm to state of interest
        oocm_subset <- subset(oocm,State==s)
        
        # convert to numeric, supress warnings due to coercion to numeric
        suppressWarnings(oocm_subset[,ci] <- as.numeric(oocm_subset[,ci]))
        
        # remove NA entries
        oocm_subset <- oocm_subset[!is.na(oocm_subset[,ci]), ]
        
        # order according to outcome
        oocm_ordered <- oocm_subset[order(oocm_subset[,ci],oocm_subset[,2]),] # col2/hospital name to break ties
        
        # match best/worst outcome to index
        if (num == "best"){
            i <- 1
        } else if (num == "worst"){
            i <- nrow(oocm_ordered)
        } else {
            i <- num
        }
        
        df <- rbind(df,data.frame(hospital=oocm_ordered[i,2],state=s))
        #print(oocm_ordered[i,2]) # col 2 is hospital name
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    df
    
}