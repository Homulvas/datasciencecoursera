rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    ## Check that state and outcome are valid
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    states <- split(data, data$State)
    ans <- data.frame(hospital=character(),
                     state=character(), 
                     stringsAsFactors=FALSE) 
    for (state in states) {
        switch(outcome,
               "pneumonia"={
                   list <- state[, c(2,23)]
               },
               "heart failure"={
                   list <- state[, c(2,17)]
               },
               "heart attack"={
                   list <- state[, c(2,11)]
               })
        names(list) <- c("name", "value")
        switch(toString(num),
               best={
                   tans <- list[order(as.numeric(list$value), list$name), ]
                   ans[state$State[1], ] <- c(tans[1,1], state$State[1])
               },
               worst={
                   tans <- list[order(as.numeric(list$value), -rank(list$name), decreasing=TRUE), ]
                   ans[state$State[1], ] <- c(tans[1,1], state$State[1])
               },
               {
                   tans <- list[order(as.numeric(list$value), list$name), ]
                   ans[state$State[1], ] <- c(tans[num,1], state$State[1])
               })
    }
    ans
}