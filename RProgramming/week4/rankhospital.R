# This is the general case of best.R - refer to it for any comments.

rankhospital <- function(state, outcome, num = "best") {
        library(data.table)
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        importantOutcomes <- outcomes[ , c(2, 7, 11, 17, 23)]
        names(importantOutcomes) <- c("Hospital", "State", "Heart Attack", "Heart Failure", "Pneumonia")
        importantOutcomes[ , 3:5] <- lapply(3:5, function(x) as.numeric(importantOutcomes[[x]]))

        if(!(state %in% importantOutcomes[ , 2])) {
                stop("Invalid state")
        }

        if(!(any(grepl(outcome, names(importantOutcomes))))) {
                stop("Invalid outcome")
        }

        goodRows <- importantOutcomes[ , 2] %like% state
        importantRows <- importantOutcomes[goodRows, ]

        index <- order(importantRows[ , outcome], importantRows[ , 1])
        semifinalData <- importantRows[index, ]
        betterData <- complete.cases(semifinalData[ , outcome])

        finalData <- semifinalData[betterData, ]


        ## Return hospital name in that state with the given rank 30-day death rate

        if(num == "best") {
                finalData[1, 1]
        } else if(num == "worst") {
                finalData[nrow(finalData), 1]
        } else {
                finalData[num, 1]
        }
}
