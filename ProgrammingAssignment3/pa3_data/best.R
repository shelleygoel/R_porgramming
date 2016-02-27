# Utility function to capitalize only first lettter of each word in a string x
simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}

# To find the best hospital in the given state 
#which has the lowest death rate for given outcome
## state : two letter string denoting state: "AZ", "MD", "NY" .....
## outcome : one of three: "heart failure" , "heart attack", "Pneumonia"
best <- function(state,outcome){
        
        # Read data
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
        
        # row index at which has minimum deaths
   
        sort_seq <- order(red_data[[outcome]],red_data$Hospital.Name)
        best_hosp <- red_data$Hospital.Name[sort_seq[1]]
        
        best_hosp  # hospital with least number of deaths for outcome
}