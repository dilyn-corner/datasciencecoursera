# This is part 2 of the week 2 quiz. 
# This function will read a directory and report the number of completely observed cases
# in each data file. It will return a data frame where the first column is the name of the
# file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        # 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form: 
        ## id nobs
        ## 1 117
        ## 2 1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the 
        ## number of complete cases

        id <- id
        files_list <- list.files(directory, full.names=TRUE)
        dat <- data.frame()
        for (i in id) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        gooddata <- complete.cases(dat)
        dat[gooddata, ]
        
        #It isn't quite finished... We should finish it
}
