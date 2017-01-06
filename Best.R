best<-function(state,outcome){
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid. If an invalid state value is passed to best, the
        ##function should throw an error via the stop function with the exact message \invalid state". If an invalid
        ##outcome value is passed to best, the function should throw an error via the stop function with the exact
        ##message \invalid outcome"
        if (!state %in% data[, "State"]) stop("invalid state")
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,c("heart attack","heart failure","pneumonia"))]
        data.state <- data[data$State==state,]
        idx <- which.min(data.state[,colName])
        data.state[idx,"Hospital.Name"]
       
}
