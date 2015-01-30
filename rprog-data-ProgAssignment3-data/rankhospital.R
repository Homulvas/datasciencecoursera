rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!is.element(state, data$State)) {
        stop("invalid state")
    }
    
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    hospitals <- data[data$State==state, ]
    switch(outcome,
           "pneumonia"={
               list <- hospitals[, c(2,23)]
           },
           "heart failure"={
               list <- hospitals[, c(2,17)]
           },
           "heart attack"={
               list <- hospitals[, c(2,11)]
           })
    names(list) <- c("name", "value")
    switch(toString(num),
           best={
               ans <- list[order(as.numeric(list$value), list$name), ]
               return(ans[1,1])
           },
           worst={
               ans <- list[order(as.numeric(list$value), -rank(list$name), decreasing=TRUE), ]
               return(ans[1,1])
           },
           {
               ans <- list[order(as.numeric(list$value), list$name), ]
               return(ans[num,1])
           })
}