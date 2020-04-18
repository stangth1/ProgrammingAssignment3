rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    
    oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    if (is.element(state,oocm$State) == FALSE) {
        stop("State not valid")
    }
    
    possible_outcomes  <- c("heart attack", "heart failure", "pneumonia")
    if (is.element(outcome,possible_outcomes) == FALSE) {
        stop("outcome not valid")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    #subsetting oocm to state of interest
    oocm_subset <- subset(oocm,State==state)
    
    # get the column index based on outcome
    column_index <- data.frame(condition = c('heart attack','heart failure','pneumonia'),col_index=c(11,17,23))
    ci <- column_index$col_index[column_index$condition==outcome]
    
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
    
    oocm_ordered[i,2] # col 2 is hospital name
}
