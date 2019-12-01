# This is part 1 of the week 2 quiz. 
# This function will find the mean of a pollutant (from two possible options) across any
# specified number of datasets (the included .csv files). 

# The function takes three arguments, 'directory', 'pollutant', and 'id'. 


pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating 
        ## the location of the CSV files
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the 
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers 
        ## to be used 

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!

        files_list <- list.files(directory, full.names=TRUE)    
        dat <- data.frame()                                      
        for (i in id) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        mean(dat[[pollutant]], na.rm=TRUE)

}
