best <- function(state, outcome) {
    
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
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    #subsetting oocm to state of interest
    oocm_subset <- subset(oocm,State==state)
    
    # get the column index based on outcome
    column_index <- data.frame(condition = c('heart attack','heart failure','pneumonia'),col_index=c(11,17,23))
    ci <- column_index$col_index[column_index$condition==outcome]
    
    # convert to numeric, supress warnings due to coercion to numeric
    suppressWarnings(oocm_subset[,ci] <- as.numeric(oocm_subset[,ci]))
    
    # get the hospital name where outcome is minimal
    best_hospitals <- oocm_subset$Hospital.Name[oocm_subset[[ci]] == min(oocm_subset[[ci]],na.rm=TRUE)]

    # tie breack: alphabetically the first
    sort(best_hospitals)[1]
    
}