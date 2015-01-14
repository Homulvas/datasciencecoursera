corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    id <- 1:332
    
    ans <- numeric()
    
    count <- 0
    
    for (i in id) {
        num <- formatC(i, width=3, flag="0")
        file <- read.csv(paste(directory, "/", num, ".csv",sep = ""))
        
        
        complete <- complete.cases(file)
        
        if (sum(complete) > threshold) {
            tem <- cor(x=file$sulfate, y=file$nitrate, use="pairwise.complete.obs")
            
            count <- count + 1
            
            ans <- append(ans, tem)
        }
    
    }
    
    ans
}