best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!is.element(state, data$State)) {
        stop("invalid state")
    }
    
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    ## State
    hospitals <- data[data$State==state, ]
    switch(outcome,
           "pneumonia"={
               list <- as.numeric(hospitals[, 23])
           },
           "heart failure"={
               list <- as.numeric(hospitals[, 17])
           },
           "heart attack"={
               list <- as.numeric(hospitals[, 11])
           })
    lowest <- which(list == min(list, na.rm=TRUE))
    ans <- hospitals$Hospital.Name[lowest]
    ## No alpbabetical ordering
    ans
}