pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    tsum <- 0
    tlength <- 0
    for (i in id) {
        num <- formatC(i, width=3, flag="0")
        file <- read.csv(paste(directory, "/", num, ".csv",sep = ""))
        
        
        if (pollutant == "sulfate") {
            sulfate <- file[2]
            complete<- sulfate[!is.na(sulfate)]
            
            tsum <- tsum + sum(complete)
            tlength <- tlength + length(complete)
        } else if (pollutant == "nitrate") {
            nitrate <- file[3]      
            complete <- nitrate[!is.na(nitrate)]

            tsum <- tsum + sum(complete)
            tlength <- tlength + length(complete)
        }
    }

    tsum / tlength
    
}