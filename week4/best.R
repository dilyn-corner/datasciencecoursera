## This program will find the best hospital in a state for any particular outcome. 
## The outcomes are pulled from a data set that I assume you have and know about...
## I don't want to explain much else about this.


## Apparently using read.csv with na.strings="Not Available" and stringsAsFactors=FALSE
## will be very helpful for actually importing the data *and* with sorting it.

## There is also apparently a names() function that can make things easier to work with.
## For instance, names(my_data) <- c("hospital", "state", "outcome")
## df[, c(2,7,column_index)] if column_index is the index for all the outcome columns.

best <- function(state, outcome) {
        library(data.table)
        ## Read outcome data
        outcomes <- read.csv("~/builds/git/datasciencecoursera/week4/outcome-of-care-measures.csv", colClasses = "character")

        ## This will generate our data table that we truly care about, and then
        ## it will give each column relevant names for more obvious manipulation, and finally
        ## convert our 3 data columns back to numeric, because they change when we rename the column.
        importantOutcomes <- outcomes[ , c(2, 7, 11, 17, 23)]
        names(importantOutcomes) <- c("Hospital", "State", "Heart Attack", "Heart Failure", "Pneumonia")
        importantOutcomes[ , 3:5] <- lapply(3:5, function(x) as.numeric(importantOutcomes[[x]]))

        ## Check that state and outcome are valid
        if(!(state %in% importantOutcomes[ , 2])) {
                   stop("Invalid state!")
        }
        ## Use grepl to avoid annoying headaches involved with naming columns changing
        ## every element to a string?
        if(!(any(grepl(outcome, names(importantOutcomes))))) {
                   stop("Invalid outcome!")
        }


        ## Now that we've determined our state and outcome are valid ones, we can filter our list.
        ## First, subset our outcomes with just the state specified. 

        goodRows <- importantOutcomes[ , 2] %like% state
        importantRows <- importantOutcomes[goodRows, ]


        ## Now we need to sort this newly created table by the outcome we want. 

        ## We create an index by which we can then reorder importantRows with. 
        ## index takes our importantRows table and orders the requisite columns in the outcome column
        ## and uses the hospital name column to break ties. 

        index <- order(importantRows[ , outcome], importantRows[ , 1])
        semifinalData <- importantRows[index, ]
        betterData <- complete.cases(semifinalData[ , outcome])
        finalData <- semifinalData[betterData, ]

        ## We can now simply look at the data by calling the rows of importantData given by the index.
        ## Alternatively, we can merely print the first row, first column entry. 


        finalData[1,1]
}
