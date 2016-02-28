rankall <- function(outcome, num="best"){
        
        data <- read.csv("outcome-of-care-measures.csv",
                         na.strings="Not Available",stringsAsFactors = F)
        
        # column nos in data frame data which contain values of outcomes in consideration
        outcomes <- c("Heart Attack" = 11, "Heart Failure" = 17, "Pneumonia" = 23 )
        
        # renaming required columns of data to more simpler names
        for (a in names(outcomes)){
                names(data)[outcomes[[a]]] <- gsub(" ",".",a)
        }
        
        
        # incorrect outcome
        if (!(tolower(outcome) %in% tolower(names(outcomes))) ){
                stop("invalid outcome")
        }
        
        # converting outcome to corresponding column name
        outcome <- gsub(" ",".",simpleCap(outcome))
        
        # Reducing data to three columns: State,Hospital name, outcome 
        
}