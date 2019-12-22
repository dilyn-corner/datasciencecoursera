add <- function(x, y) {
        x + y 
}               # in R, the function returns the last expression

above10 <- function(x) {
        use <- x > 10
        x[use]
}

above <- function(x, n = 10) {
        use <- x > n
        x[use]
}               # by setting n = 10, we are choosing a default value for n. It can be overwritten.

columnmean <- function(x, removeNA = TRUE) {
        nc <- ncol(x)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(x[, i], na.rm = removeNA)
        }
        mean
}
