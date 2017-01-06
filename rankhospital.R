rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
        ## Check that state and outcome are valid
        if (!state %in% data[, "State"]) stop("invalid state")
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colName <- fullColName[match(outcome,c("heart attack","heart failure","pneumonia"))]
        data.state <- data[data$State==state,]
        ##data.state.sort<-data.state[order(data.state$colName,data.state$Hospital.Name, na.last=NA),]
        data.state.sort<-data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
        if (num=="best") num = 1
        if (num=='worst') num = nrow(data.state.sort)
        if (is.numeric(num) & num > nrow(data.state.sort)) return(NA)
       
        hospital<-data.state.sort[as.integer(num),"Hospital.Name"]
        hospital
}
