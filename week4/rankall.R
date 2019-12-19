## Refer to best.R for information on how the first bit of code runs.

rankall <- function(outcome, num = "best") {
        library(data.table)

        outcomes <- read.csv("~builds/git/datasciencecoursera/week4/outcome-of-care-measures.csv", colClasses = "character")

        importantOutcomes <- outcomes[ , c(2, 7, 11, 17, 23)]
        names(importantOutcomes) <- c("Hospital", "State", "Heart Attack", "Heart Failure", "Pneumonia")
        importantOutcomes[ , 3:5] <- lapply(3:5, function(x) as.numeric(importantOutcomes[[x]]))

        if(!(state %in% importantOutcomes[ , 2])) {
                stop("Invalid state!")
        }
        if(!(any(grepl(outcome, names(importantOutcomes))))) {
                stop("Invalid outcome!")
        }

       index <- with(importantRows, order(importantRows[ , outcome], importantRows[ , 1], decreasing = TRUE))
       semifinalData <- importantRows[index, ]
       betterData <- complete.cases(semifinalData[ , outcome])
       finalData <- semifinalData[betterData, ]


        ## For each state, find the hospital of the given rank

        ## Return a data frame with the hospital names and the (abbreviated) state name

}
