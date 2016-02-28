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
        data <- subset(data,select = c("Hospital.Name", "State", outcome))
        
        # split data into one data frame per state
        state_data <- split(data,data$State)
        
        # remove missing data from each data frame
        state_data <- lapply(state_data,na.omit)
        
        # getting a vector of hospitals names with rank num in each state
        best_hosp <- sapply(state_data,rank_hosp,outcome,num)
        
        state_names <- names(state_data)
        
        ranked_hosp = data.frame(hospital=best_hosp,state=state_names,
                                 row.names=state_names)
        
        ranked_hosp
}


rank_hosp <- function(data,outcome,num){
        
        if (num == "best") num <- 1
        
        if (is.integer(num) & num > nrow(data)) return(NA)
        
        sort_seq <- order(data[[outcome]], data$Hospital.Name)
        
        if (num == "worst") 
                best_hosp <- data$Hospital.Name[tail(sort_seq,n=1)]
        
        else
                best_hosp <- data$Hospital.Name[sort_seq[num]]
        
        best_hosp  # hospital with ranking "num" for outcome 
        
}