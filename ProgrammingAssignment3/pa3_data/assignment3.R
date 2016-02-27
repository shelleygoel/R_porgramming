best <- function(state,outcome){
        data <- read.csv("outcome-of-care-measures.csv")
        if (!(toupper(state) %in% data$state) ){
                stop("invalid state")
        }
        if (!(tolower(outcome) %in% c("hear attack","heart failure","pneumonia")) ){
                stop("invalid outcome")
        }
        
}