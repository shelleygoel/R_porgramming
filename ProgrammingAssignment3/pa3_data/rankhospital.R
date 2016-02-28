rankhospital <- function(state,outcome,num="best"){
         
        data <- read.csv("outcome-of-care-measures.csv",
                         na.strings="Not Available",stringsAsFactors = F)
        
        # column nos in data frame data which contain values of outcomes in consideration
        outcomes <- c("Heart Attack" = 11, "Heart Failure" = 17, "Pneumonia" = 23 )
        
        # renaming required columns of data to more simpler names
        for (a in names(outcomes)){
                names(data)[outcomes[[a]]] <- gsub(" ",".",a)
        }
        
        # incorrect state
        if (!(toupper(state) %in% toupper(data$State))){
                stop("invalid state")
        }
        
        # incorrect outcome
        if (!(tolower(outcome) %in% tolower(names(outcomes))) ){
                stop("invalid outcome")
        }
        
        # converting outcome to corresponding column name
        outcome <- gsub(" ",".",simpleCap(outcome))
        
        # Reducing data to three columns: State,Hospital name, outcome 
        red_data <- subset(data, State == state, 
                           select = c("Hospital.Name", "State", outcome))
        
        red_data <- na.omit(red_data)
        
        if (num == "best") num <- 1
        if (is.integer(num) & num > nrow(red_data)) return(NA)
        
        # ranking of hospitals according for outcome
        sort_seq <- order(red_data[[outcome]],red_data$Hospital.Name)
        
        if (num == "worst") 
                best_hosp <- red_data$Hospital.Name[tail(sort_seq,n=1)]
        
        else
                best_hosp <- red_data$Hospital.Name[sort_seq[num]]
        
        best_hosp  # hospital in given state with ranking "num" for outcome 
}